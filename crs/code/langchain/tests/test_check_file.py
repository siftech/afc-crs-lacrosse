import pytest

from lacrosse_llm.check_file import ResponseError, parse_vuln_resp

test_response = """
Let's analyze the provided code step by step to determine if it exhibits CWE-20: Improper Input Validation.

1. **Overview of the Code:**
   - The code is part of TIPC (Transparent Inter-Process Communication) for handling cryptographic operations.
   - It includes various structures and definitions for managing cryptographic keys, AEAD (Authenticated Encryption with Associated Data), and other related operations.
   - The function `tipc_crypto_key_rcv` is responsible for receiving a cryptographic key from a message and scheduling the work to attach the key to the corresponding RX (receiver) crypto structure.

2. **Focus on `tipc_crypto_key_rcv`:**
   - This function extracts the session key from a message and allocates memory for it.
   - It performs some checks and then copies the key data from the message into the allocated memory.
   - Finally, it schedules a work item to attach the key to the RX crypto structure.

3. **Key Points to Check for CWE-20:**
   - **Input Validation:** The main input to this function is the message (`hdr`) that contains the key data.
   - **Memory Allocation:** The function allocates memory for the key using `kmalloc`.
   - **Sanity Check:** There is a sanity check to ensure the size of the key is correct.

4. **Detailed Analysis of Input Handling:**
   - The function checks if the `key_gen` in the message is already present in the RX crypto structure.
   - It allocates memory for the key using the size obtained from the message (`size = msg_data_sz(hdr)`).
   - After allocating memory, it copies the key data from the message to the allocated memory.
   - There is a sanity check to ensure the size of the key matches the expected size (`tipc_aead_key_size(skey)`).

5. **Potential Issues:**
   - **Sanity Check:** The sanity check only verifies the size of the key after allocating memory and copying data.
   - **Lack of Validation Before Memory Operations:** There is no validation of `size` before calling `kmalloc`. If `size` is excessively large, it could lead to memory exhaustion (CWE-789).
   - **No Check on `msg_data_sz(hdr)`:** There is no validation to ensure `msg_data_sz(hdr)` is within expected bounds before using it for memory allocation and data copying.
   - **No Check on `msg_key_gen(hdr)`:** The function assumes `msg_key_gen(hdr)` is a valid value without validation.

Based on the analysis:

**Question: Does the following code exhibit CWE-20?**

**Answer: YES**

The code does not adequately validate the input before processing it, particularly the size of the key data (`msg_data_sz(hdr)`) and the key generation (`msg_key_gen(hdr)`). This could lead to improper memory allocation and potential vulnerabilities related to memory exhaustion and other issues.
"""


def test_parse_response():
    assert parse_vuln_resp(test_response)
    assert parse_vuln_resp(test_response.replace("**Answer: YES**", "Answer: YES"))
    assert not parse_vuln_resp(test_response.replace("**Answer: YES**", "Answer: No"))
    assert not parse_vuln_resp(test_response.replace("**Answer: YES**", "Answer: nO"))
    assert not parse_vuln_resp(test_response.replace("**Answer: YES**", "**Answer: NO**"))
    with pytest.raises(ResponseError):
        parse_vuln_resp(test_response.replace("**Answer: YES**", "**Answer: Bill**"))
