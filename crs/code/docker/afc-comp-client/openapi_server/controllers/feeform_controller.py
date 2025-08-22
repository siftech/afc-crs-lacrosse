import connexion
from typing import Dict
from typing import Tuple
from typing import Union

from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.models.types_freeform_response import TypesFreeformResponse  # noqa: E501
from openapi_server.models.types_freeform_submission import TypesFreeformSubmission  # noqa: E501
from openapi_server import util


def v1_task_task_id_freeform_post(task_id, body):  # noqa: E501
    """Submit Freeform

    submits a freeform pov # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param payload: Submission Body
    :type payload: dict | bytes

    :rtype: Union[TypesFreeformResponse, Tuple[TypesFreeformResponse, int], Tuple[TypesFreeformResponse, int, Dict[str, str]]
    """
    payload = body
    if connexion.request.is_json:
        payload = TypesFreeformSubmission.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
