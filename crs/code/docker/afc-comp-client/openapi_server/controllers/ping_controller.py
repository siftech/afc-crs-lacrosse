import connexion
from typing import Dict
from typing import Tuple
from typing import Union

from openapi_server.models.types_ping_response import TypesPingResponse  # noqa: E501
from openapi_server import util


def v1_ping_get():  # noqa: E501
    """Test authentication creds and network connectivity

    Test authentication creds and network connectivity # noqa: E501


    :rtype: Union[TypesPingResponse, Tuple[TypesPingResponse, int], Tuple[TypesPingResponse, int, Dict[str, str]]
    """
    return 'do some magic!'
