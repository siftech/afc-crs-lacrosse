import connexion
from typing import Dict
from typing import Tuple
from typing import Union

from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.models.types_patch_submission import TypesPatchSubmission  # noqa: E501
from openapi_server.models.types_patch_submission_response import TypesPatchSubmissionResponse  # noqa: E501
from openapi_server import util


def v1_task_task_id_patch_patch_id_get(task_id, patch_id):  # noqa: E501
    """Patch Status

    yield the status of patch testing # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param patch_id: Patch ID
    :type patch_id: str
    :type patch_id: str

    :rtype: Union[TypesPatchSubmissionResponse, Tuple[TypesPatchSubmissionResponse, int], Tuple[TypesPatchSubmissionResponse, int, Dict[str, str]]
    """
    return 'do some magic!'


def v1_task_task_id_patch_post(task_id, body):  # noqa: E501
    """Submit Patch

    submit a patch for testing # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param payload: Payload
    :type payload: dict | bytes

    :rtype: Union[TypesPatchSubmissionResponse, Tuple[TypesPatchSubmissionResponse, int], Tuple[TypesPatchSubmissionResponse, int, Dict[str, str]]
    """
    payload = body
    if connexion.request.is_json:
        payload = TypesPatchSubmission.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
