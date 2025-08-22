import connexion
from typing import Dict
from typing import Tuple
from typing import Union

from openapi_server.models.types_bundle_submission import TypesBundleSubmission  # noqa: E501
from openapi_server.models.types_bundle_submission_response import TypesBundleSubmissionResponse  # noqa: E501
from openapi_server.models.types_bundle_submission_response_verbose import TypesBundleSubmissionResponseVerbose  # noqa: E501
from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server import util


def v1_task_task_id_bundle_bundle_id_delete(task_id, bundle_id):  # noqa: E501
    """Delete Bundle

    delete a bundle # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param bundle_id: Bundle ID
    :type bundle_id: str
    :type bundle_id: str

    :rtype: Union[str, Tuple[str, int], Tuple[str, int, Dict[str, str]]
    """
    return 'do some magic!'


def v1_task_task_id_bundle_bundle_id_get(task_id, bundle_id):  # noqa: E501
    """Get Bundle

    get a bundle # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param bundle_id: Bundle ID
    :type bundle_id: str
    :type bundle_id: str

    :rtype: Union[TypesBundleSubmissionResponseVerbose, Tuple[TypesBundleSubmissionResponseVerbose, int], Tuple[TypesBundleSubmissionResponseVerbose, int, Dict[str, str]]
    """
    return 'do some magic!'


def v1_task_task_id_bundle_bundle_id_patch(task_id, bundle_id, body):  # noqa: E501
    """Update Bundle

    updates a bundle # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param bundle_id: Bundle ID
    :type bundle_id: str
    :type bundle_id: str
    :param payload: Submission Body
    :type payload: dict | bytes

    :rtype: Union[TypesBundleSubmissionResponseVerbose, Tuple[TypesBundleSubmissionResponseVerbose, int], Tuple[TypesBundleSubmissionResponseVerbose, int, Dict[str, str]]
    """
    payload = body
    if connexion.request.is_json:
        payload = TypesBundleSubmission.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def v1_task_task_id_bundle_post(task_id, body):  # noqa: E501
    """Submit Bundle

    submits a bundle # noqa: E501

    :param task_id: Task ID
    :type task_id: str
    :type task_id: str
    :param payload: Submission Body
    :type payload: dict | bytes

    :rtype: Union[TypesBundleSubmissionResponse, Tuple[TypesBundleSubmissionResponse, int], Tuple[TypesBundleSubmissionResponse, int, Dict[str, str]]
    """
    payload = body
    if connexion.request.is_json:
        payload = TypesBundleSubmission.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
