import unittest

from flask import json

from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.models.types_sarif_assessment_response import TypesSarifAssessmentResponse  # noqa: E501
from openapi_server.models.types_sarif_assessment_submission import TypesSarifAssessmentSubmission  # noqa: E501
from openapi_server.test import BaseTestCase


class TestBroadcastSarifAssessmentController(BaseTestCase):
    """BroadcastSarifAssessmentController integration test stubs"""

    def test_v1_task_task_id_broadcast_sarif_assessment_broadcast_sarif_id_post(self):
        """Test case for v1_task_task_id_broadcast_sarif_assessment_broadcast_sarif_id_post

        Submit a SARIF Assessment
        """
        payload = openapi_server.TypesSarifAssessmentSubmission()
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/broadcast-sarif-assessment/{broadcast_sarif_id}'.format(task_id='task_id_example', broadcast_sarif_id='broadcast_sarif_id_example'),
            method='POST',
            headers=headers,
            data=json.dumps(payload),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
