import connexion
from typing import Dict
from typing import Tuple
from typing import Union

from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.models.types_sarif_submission import TypesSARIFSubmission  # noqa: E501
from openapi_server.models.types_sarif_submission_response import TypesSARIFSubmissionResponse  # noqa: E501
from openapi_server import util


def v1_task_task_id_submitted_sarif_post(task_id, body):  # noqa: E501
    """Submit a CRS generated SARIF

    Submit a CRS generated SARIF # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param payload: Submission body
    :type payload: dict | bytes

    :rtype: Union[TypesSARIFSubmissionResponse, Tuple[TypesSARIFSubmissionResponse, int], Tuple[TypesSARIFSubmissionResponse, int, Dict[str, str]]
    """
    payload = body
    if connexion.request.is_json:
        payload = TypesSARIFSubmission.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
