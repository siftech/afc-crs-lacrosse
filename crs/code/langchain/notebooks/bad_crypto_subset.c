// SPDX-License-Identifier: GPL-2.0
/*
 * net/tipc/crypto.c: TIPC crypto for key handling & packet en/decryption
 *
 * Copyright (c) 2019, Ericsson AB
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <crypto/aead.h>
#include <crypto/aes.h>
#include <crypto/rng.h>
#include "crypto.h"
#include "msg.h"
#include "bcast.h"

#define TIPC_TX_GRACE_PERIOD	msecs_to_jiffies(5000) /* 5s */
#define TIPC_TX_LASTING_TIME	msecs_to_jiffies(10000) /* 10s */
#define TIPC_RX_ACTIVE_LIM	msecs_to_jiffies(3000) /* 3s */
#define TIPC_RX_PASSIVE_LIM	msecs_to_jiffies(15000) /* 15s */

#define TIPC_MAX_TFMS_DEF	10
#define TIPC_MAX_TFMS_LIM	1000

#define TIPC_REKEYING_INTV_DEF	(60 * 24) /* default: 1 day */

/*
 * TIPC Key ids
 */
enum {
	KEY_MASTER = 0,
	KEY_MIN = KEY_MASTER,
	KEY_1 = 1,
	KEY_2,
	KEY_3,
	KEY_MAX = KEY_3,
};

/*
 * TIPC Crypto statistics
 */
enum {
	STAT_OK,
	STAT_NOK,
	STAT_ASYNC,
	STAT_ASYNC_OK,
	STAT_ASYNC_NOK,
	STAT_BADKEYS, /* tx only */
	STAT_BADMSGS = STAT_BADKEYS, /* rx only */
	STAT_NOKEYS,
	STAT_SWITCHES,

	MAX_STATS,
};

/* TIPC crypto statistics' header */
static const char *hstats[MAX_STATS] = {"ok", "nok", "async", "async_ok",
					"async_nok", "badmsgs", "nokeys",
					"switches"};

/* Max TFMs number per key */
int sysctl_tipc_max_tfms __read_mostly = TIPC_MAX_TFMS_DEF;
/* Key exchange switch, default: on */
int sysctl_tipc_key_exchange_enabled __read_mostly = 1;

/*
 * struct tipc_key - TIPC keys' status indicator
 *
 *         7     6     5     4     3     2     1     0
 *      +-----+-----+-----+-----+-----+-----+-----+-----+
 * key: | (reserved)|passive idx| active idx|pending idx|
 *      +-----+-----+-----+-----+-----+-----+-----+-----+
 */
struct tipc_key {
#define KEY_BITS (2)
#define KEY_MASK ((1 << KEY_BITS) - 1)
	union {
		struct {
#if defined(__LITTLE_ENDIAN_BITFIELD)
			u8 pending:2,
			   active:2,
			   passive:2, /* rx only */
			   reserved:2;
#elif defined(__BIG_ENDIAN_BITFIELD)
			u8 reserved:2,
			   passive:2, /* rx only */
			   active:2,
			   pending:2;
#else
#error  "Please fix <asm/byteorder.h>"
#endif
		} __packed;
		u8 keys;
	};
};

/**
 * struct tipc_tfm - TIPC TFM structure to form a list of TFMs
 * @tfm: cipher handle/key
 * @list: linked list of TFMs
 */
struct tipc_tfm {
	struct crypto_aead *tfm;
	struct list_head list;
};

/**
 * struct tipc_aead - TIPC AEAD key structure
 * @tfm_entry: per-cpu pointer to one entry in TFM list
 * @crypto: TIPC crypto owns this key
 * @cloned: reference to the source key in case cloning
 * @users: the number of the key users (TX/RX)
 * @salt: the key's SALT value
 * @authsize: authentication tag size (max = 16)
 * @mode: crypto mode is applied to the key
 * @hint: a hint for user key
 * @rcu: struct rcu_head
 * @key: the aead key
 * @gen: the key's generation
 * @seqno: the key seqno (cluster scope)
 * @refcnt: the key reference counter
 */
struct tipc_aead {
#define TIPC_AEAD_HINT_LEN (5)
	struct tipc_tfm * __percpu *tfm_entry;
	struct tipc_crypto *crypto;
	struct tipc_aead *cloned;
	atomic_t users;
	u32 salt;
	u8 authsize;
	u8 mode;
	char hint[2 * TIPC_AEAD_HINT_LEN + 1];
	struct rcu_head rcu;
	struct tipc_aead_key *key;
	u16 gen;

	atomic64_t seqno ____cacheline_aligned;
	refcount_t refcnt ____cacheline_aligned;

} ____cacheline_aligned;

/**
 * struct tipc_crypto_stats - TIPC Crypto statistics
 * @stat: array of crypto statistics
 */
struct tipc_crypto_stats {
	unsigned int stat[MAX_STATS];
};

/**
 * struct tipc_crypto - TIPC TX/RX crypto structure
 * @net: struct net
 * @node: TIPC node (RX)
 * @aead: array of pointers to AEAD keys for encryption/decryption
 * @peer_rx_active: replicated peer RX active key index
 * @key_gen: TX/RX key generation
 * @key: the key states
 * @skey_mode: session key's mode
 * @skey: received session key
 * @wq: common workqueue on TX crypto
 * @work: delayed work sched for TX/RX
 * @key_distr: key distributing state
 * @rekeying_intv: rekeying interval (in minutes)
 * @stats: the crypto statistics
 * @name: the crypto name
 * @sndnxt: the per-peer sndnxt (TX)
 * @timer1: general timer 1 (jiffies)
 * @timer2: general timer 2 (jiffies)
 * @working: the crypto is working or not
 * @key_master: flag indicates if master key exists
 * @legacy_user: flag indicates if a peer joins w/o master key (for bwd comp.)
 * @nokey: no key indication
 * @flags: combined flags field
 * @lock: tipc_key lock
 */
struct tipc_crypto {
	struct net *net;
	struct tipc_node *node;
	struct tipc_aead __rcu *aead[KEY_MAX + 1];
	atomic_t peer_rx_active;
	u16 key_gen;
	struct tipc_key key;
	u8 skey_mode;
	struct tipc_aead_key *skey;
	struct workqueue_struct *wq;
	struct delayed_work work;
#define KEY_DISTR_SCHED		1
#define KEY_DISTR_COMPL		2
	atomic_t key_distr;
	u32 rekeying_intv;

	struct tipc_crypto_stats __percpu *stats;
	char name[48];

	atomic64_t sndnxt ____cacheline_aligned;
	unsigned long timer1;
	unsigned long timer2;
	union {
		struct {
			u8 working:1;
			u8 key_master:1;
			u8 legacy_user:1;
			u8 nokey: 1;
		};
		u8 flags;
	};
	spinlock_t lock; /* crypto lock */

} ____cacheline_aligned;

/* struct tipc_crypto_tx_ctx - TX context for callbacks */
struct tipc_crypto_tx_ctx {
	struct tipc_aead *aead;
	struct tipc_bearer *bearer;
	struct tipc_media_addr dst;
};

/* struct tipc_crypto_rx_ctx - RX context for callbacks */
struct tipc_crypto_rx_ctx {
	struct tipc_aead *aead;
	struct tipc_bearer *bearer;
};

static struct tipc_aead *tipc_aead_get(struct tipc_aead __rcu *aead);
static inline void tipc_aead_put(struct tipc_aead *aead);
static void tipc_aead_free(struct rcu_head *rp);
static int tipc_aead_users(struct tipc_aead __rcu *aead);
static void tipc_aead_users_inc(struct tipc_aead __rcu *aead, int lim);
static void tipc_aead_users_dec(struct tipc_aead __rcu *aead, int lim);
static void tipc_aead_users_set(struct tipc_aead __rcu *aead, int val);
static struct crypto_aead *tipc_aead_tfm_next(struct tipc_aead *aead);
static int tipc_aead_init(struct tipc_aead **aead, struct tipc_aead_key *ukey,
			  u8 mode);
static int tipc_aead_clone(struct tipc_aead **dst, struct tipc_aead *src);
static void *tipc_aead_mem_alloc(struct crypto_aead *tfm,
				 unsigned int crypto_ctx_size,
				 u8 **iv, struct aead_request **req,
				 struct scatterlist **sg, int nsg);
static int tipc_aead_encrypt(struct tipc_aead *aead, struct sk_buff *skb,
			     struct tipc_bearer *b,
			     struct tipc_media_addr *dst,
			     struct tipc_node *__dnode);
static void tipc_aead_encrypt_done(void *data, int err);
static int tipc_aead_decrypt(struct net *net, struct tipc_aead *aead,
			     struct sk_buff *skb, struct tipc_bearer *b);
static void tipc_aead_decrypt_done(void *data, int err);
static inline int tipc_ehdr_size(struct tipc_ehdr *ehdr);
static int tipc_ehdr_build(struct net *net, struct tipc_aead *aead,
			   u8 tx_key, struct sk_buff *skb,
			   struct tipc_crypto *__rx);
static inline void tipc_crypto_key_set_state(struct tipc_crypto *c,
					     u8 new_passive,
					     u8 new_active,
					     u8 new_pending);
static int tipc_crypto_key_attach(struct tipc_crypto *c,
				  struct tipc_aead *aead, u8 pos,
				  bool master_key);
static bool tipc_crypto_key_try_align(struct tipc_crypto *rx, u8 new_pending);
static struct tipc_aead *tipc_crypto_key_pick_tx(struct tipc_crypto *tx,
						 struct tipc_crypto *rx,
						 struct sk_buff *skb,
						 u8 tx_key);
static void tipc_crypto_key_synch(struct tipc_crypto *rx, struct sk_buff *skb);
static int tipc_crypto_key_revoke(struct net *net, u8 tx_key);
static inline void tipc_crypto_clone_msg(struct net *net, struct sk_buff *_skb,
					 struct tipc_bearer *b,
					 struct tipc_media_addr *dst,
					 struct tipc_node *__dnode, u8 type);
static void tipc_crypto_rcv_complete(struct net *net, struct tipc_aead *aead,
				     struct tipc_bearer *b,
				     struct sk_buff **skb, int err);
static void tipc_crypto_do_cmd(struct net *net, int cmd);
static char *tipc_crypto_key_dump(struct tipc_crypto *c, char *buf);
static char *tipc_key_change_dump(struct tipc_key old, struct tipc_key new,
				  char *buf);
static int tipc_crypto_key_xmit(struct net *net, struct tipc_aead_key *skey,
				u16 gen, u8 mode, u32 dnode);
static bool tipc_crypto_key_rcv(struct tipc_crypto *rx, struct tipc_msg *hdr);
static void tipc_crypto_work_tx(struct work_struct *work);
static void tipc_crypto_work_rx(struct work_struct *work);
static int tipc_aead_key_generate(struct tipc_aead_key *skey);

#define is_tx(crypto) (!(crypto)->node)
#define is_rx(crypto) (!is_tx(crypto))

#define key_next(cur) ((cur) % KEY_MAX + 1)

#define tipc_aead_rcu_ptr(rcu_ptr, lock)				\
	rcu_dereference_protected((rcu_ptr), lockdep_is_held(lock))

#define tipc_aead_rcu_replace(rcu_ptr, ptr, lock)			\
do {									\
	struct tipc_aead *__tmp = rcu_dereference_protected((rcu_ptr),	\
						lockdep_is_held(lock));	\
	rcu_assign_pointer((rcu_ptr), (ptr));				\
	tipc_aead_put(__tmp);						\
} while (0)

#define tipc_crypto_key_detach(rcu_ptr, lock)				\
	tipc_aead_rcu_replace((rcu_ptr), NULL, lock)

/**
 * tipc_crypto_key_rcv - Receive a session key
 * @rx: the RX crypto
 * @hdr: the TIPC v2 message incl. the receiving session key in its data
 *
 * This function retrieves the session key in the message from peer, then
 * schedules a RX work to attach the key to the corresponding RX crypto.
 *
 * Return: "true" if the key has been scheduled for attaching, otherwise
 * "false".
 */
static bool tipc_crypto_key_rcv(struct tipc_crypto *rx, struct tipc_msg *hdr)
{
	struct tipc_crypto *tx = tipc_net(rx->net)->crypto_tx;
	struct tipc_aead_key *skey = NULL;
	u16 key_gen = msg_key_gen(hdr);
	u32 size = msg_data_sz(hdr);
	u8 *data = msg_data(hdr);

	spin_lock(&rx->lock);
	if (unlikely(rx->skey || (key_gen == rx->key_gen && rx->key.keys))) {
		pr_err("%s: key existed <%p>, gen %d vs %d\n", rx->name,
		       rx->skey, key_gen, rx->key_gen);
		goto exit;
	}

	/* Allocate memory for the key */
	skey = kmalloc(size, GFP_ATOMIC);
	if (unlikely(!skey)) {
		pr_err("%s: unable to allocate memory for skey\n", rx->name);
		goto exit;
	}

	/* Copy key from msg data */
	skey->keylen = ntohl(*((__be32 *)(data + TIPC_AEAD_ALG_NAME)));
	memcpy(skey->alg_name, data, TIPC_AEAD_ALG_NAME);
	memcpy(skey->key, data + TIPC_AEAD_ALG_NAME + sizeof(__be32),
	       skey->keylen);

	/* Sanity check */
	if (unlikely(size != tipc_aead_key_size(skey))) {
		kfree(skey);
		skey = NULL;
		goto exit;
	}

	rx->key_gen = key_gen;
	rx->skey_mode = msg_key_mode(hdr);
	rx->skey = skey;
	rx->nokey = 0;
	mb(); /* for nokey flag */

exit:
	spin_unlock(&rx->lock);

	/* Schedule the key attaching on this crypto */
	if (likely(skey && queue_delayed_work(tx->wq, &rx->work, 0)))
		return true;

	return false;
}
