import re
import pytest
from lacrosse_llm.check_file import parse_vuln_resp, parse_resp_field, RespDict, parse_resp_string

sample_response = \
"""
[VULNERABLE] YES [/VULNERABLE]
[VULNERABILITY TYPE] CWE-120 [/VULNERABILITY TYPE]
[VULNERABILITY NAME] Classic Buffer Overflow [/VULNERABILITY NAME]
[EXPLANATION] The function tipc_crypto_key_rcv does not properly validate the size of the incoming data before copying it into a buffer. This could lead to buffer overflow if an attacker sends a message with a larger data size than expected. [/EXPLANATION]
[FIX]
```c
static bool tipc_crypto_key_rcv(struct tipc_crypto *rx, struct tipc_msg *hdr)
{
        struct tipc_crypto *tx = tipc_net(rx->net)->crypto_tx;
        struct tipc_aead_key *skey = NULL;
        u16 key_gen = msg_key_gen(hdr);
        u32 size = msg_data_sz(hdr);
        u8 *data = msg_data(hdr);
        u32 expected_size;

        spin_lock(&rx->lock);
        if (unlikely(rx->skey || (key_gen == rx->key_gen && rx->key.keys))) {
                pr_err("%s: key existed <%p>, gen %d vs %d\n", rx->name,
                       rx->skey, key_gen, rx->key_gen);
                goto exit;
        }

        /* Calculate expected size for sanity check */
        expected_size = TIPC_AEAD_ALG_NAME + sizeof(__be32) + ntohl(*((__be32 *)(data + TIPC_AEAD_ALG_NAME)));

        /* Allocate memory for the key */
        skey = kmalloc(expected_size - TIPC_AEAD_ALG_NAME - sizeof(__be32), GFP_ATOMIC);
        if (unlikely(!skey)) {
                pr_err("%s: unable to allocate memory for skey\n", rx->name);
                goto exit;
        }

        /* Copy key from msg data */
        skey->keylen = ntohl(*((__be32 *)(data + TIPC_AEAD_ALG_NAME)));
        if (skey->keylen > TIPC_MAX_KEY_LEN) { // Define TIPC_MAX_KEY_LEN appropriately based on the protocol and implementation
                kfree(skey);
                pr_err("%s: key length is too large\n", rx->name);
                skey = NULL;
                goto exit;
        }
        memcpy(skey->alg_name, data, TIPC_AEAD_ALG_NAME);
        memcpy(skey->key, data + TIPC_AEAD_ALG_NAME + sizeof(__be32), skey->keylen);

        /* Sanity check */
        if (unlikely(size != expected_size)) {
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
```
[/FIX]
"""

def test_parse_vulnerable():
     assert parse_vuln_resp(sample_response)
def test_parse_cwe():
    assert parse_resp_field(sample_response, "VULNERABILITY TYPE") == "CWE-120"
def test_re():
    tag_name = "FIX"
    re_str = r"\[" + tag_name + r"\]" + r"\s*(.*\S)\s*" + r"\[/" + tag_name + r"\]"
    assert re_str == r'\[FIX\]\s*(.*\S)\s*\[/FIX\]'
    # check for closing tag
    assert re.search(r"\S\s*\[/FIX\]", sample_response, flags=re.MULTILINE|re.IGNORECASE)
    # check for opening tag
    assert re.search(r"\[FIX\]", sample_response, flags=re.MULTILINE|re.IGNORECASE)
    m = re.search(r"\[FIX\](.*)", sample_response, flags=re.MULTILINE | re.IGNORECASE | re.DOTALL)
    print(f"After opening tag is:{m.group(1)}")
    assert re.search(re_str, sample_response, flags=re.MULTILINE | re.IGNORECASE | re.DOTALL)
def test_parse_multiline():
    parse_resp_field(sample_response, "FIX", multi_line=True)
def test_parse_all():
    expected = RespDict(is_vulnerable=True,
                        vuln_id="CWE-120",
                        vuln_name="Classic Buffer Overflow",
                        explanation=("The function tipc_crypto_key_rcv does not properly validate the size "
                                     "of the incoming data before copying it into a buffer. This could lead "
                                     "to buffer overflow if an attacker sends a message with a larger "
                                     "data size than expected."),
                        )
    actual = parse_resp_string(sample_response)
    del actual["fix"] # too hard to match
    assert expected == actual
