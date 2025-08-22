import unittest

from flask import json

from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.models.types_sarif_submission import TypesSARIFSubmission  # noqa: E501
from openapi_server.models.types_sarif_submission_response import TypesSARIFSubmissionResponse  # noqa: E501
from openapi_server.test import BaseTestCase


class TestSubmittedSarifController(BaseTestCase):
    """SubmittedSarifController integration test stubs"""

    def test_v1_task_task_id_submitted_sarif_post(self):
        """Test case for v1_task_task_id_submitted_sarif_post

        Submit a CRS generated SARIF
        """
        payload = openapi_server.TypesSARIFSubmission()
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/submitted-sarif'.format(task_id='task_id_example'),
            method='POST',
            headers=headers,
            data=json.dumps(payload),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
