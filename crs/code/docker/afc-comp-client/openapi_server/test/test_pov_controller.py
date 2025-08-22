import unittest

from flask import json

from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.models.types_pov_submission import TypesPOVSubmission  # noqa: E501
from openapi_server.models.types_pov_submission_response import TypesPOVSubmissionResponse  # noqa: E501
from openapi_server.test import BaseTestCase


class TestPovController(BaseTestCase):
    """PovController integration test stubs"""

    def test_v1_task_task_id_pov_post(self):
        """Test case for v1_task_task_id_pov_post

        Submit Vulnerability
        """
        payload = openapi_server.TypesPOVSubmission()
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/pov'.format(task_id='task_id_example'),
            method='POST',
            headers=headers,
            data=json.dumps(payload),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_v1_task_task_id_pov_pov_id_get(self):
        """Test case for v1_task_task_id_pov_pov_id_get

        Vulnerability Status
        """
        headers = { 
            'Accept': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/pov/{pov_id}'.format(task_id='task_id_example', pov_id='pov_id_example'),
            method='GET',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
