import connexion
from typing import Dict
from typing import Tuple
from typing import Union

from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.models.types_sarif_assessment_response import TypesSarifAssessmentResponse  # noqa: E501
from openapi_server.models.types_sarif_assessment_submission import TypesSarifAssessmentSubmission  # noqa: E501
from openapi_server import util


def v1_task_task_id_broadcast_sarif_assessment_broadcast_sarif_id_post(task_id, broadcast_sarif_id, body):  # noqa: E501
    """Submit a SARIF Assessment

    Submit a SARIF assessment # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param broadcast_sarif_id: Broadcast SARIF ID
    :type broadcast_sarif_id: str
    :type broadcast_sarif_id: str
    :param payload: Submission body
    :type payload: dict | bytes

    :rtype: Union[TypesSarifAssessmentResponse, Tuple[TypesSarifAssessmentResponse, int], Tuple[TypesSarifAssessmentResponse, int, Dict[str, str]]
    """
    payload = body
    if connexion.request.is_json:
        payload = TypesSarifAssessmentSubmission.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
