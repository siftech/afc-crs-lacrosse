import connexion
from typing import Dict
from typing import Tuple
from typing import Union

from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.models.types_pov_submission import TypesPOVSubmission  # noqa: E501
from openapi_server.models.types_pov_submission_response import TypesPOVSubmissionResponse  # noqa: E501
from openapi_server import util


def v1_task_task_id_pov_post(task_id, body):  # noqa: E501
    """Submit Vulnerability

    submit a vulnerability for testing # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param payload: Submission body
    :type payload: dict | bytes

    :rtype: Union[TypesPOVSubmissionResponse, Tuple[TypesPOVSubmissionResponse, int], Tuple[TypesPOVSubmissionResponse, int, Dict[str, str]]
    """
    payload = body
    if connexion.request.is_json:
        payload = TypesPOVSubmission.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def v1_task_task_id_pov_pov_id_get(task_id, pov_id):  # noqa: E501
    """Vulnerability Status

    yield the status of vuln testing # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param pov_id: POV ID
    :type pov_id: str
    :type pov_id: str

    :rtype: Union[TypesPOVSubmissionResponse, Tuple[TypesPOVSubmissionResponse, int], Tuple[TypesPOVSubmissionResponse, int, Dict[str, str]]
    """
    return 'do some magic!'
