import unittest

from flask import json

from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.models.types_patch_submission import TypesPatchSubmission  # noqa: E501
from openapi_server.models.types_patch_submission_response import TypesPatchSubmissionResponse  # noqa: E501
from openapi_server.test import BaseTestCase


class TestPatchController(BaseTestCase):
    """PatchController integration test stubs"""

    def test_v1_task_task_id_patch_patch_id_get(self):
        """Test case for v1_task_task_id_patch_patch_id_get

        Patch Status
        """
        headers = { 
            'Accept': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/patch/{patch_id}'.format(task_id='task_id_example', patch_id='patch_id_example'),
            method='GET',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_v1_task_task_id_patch_post(self):
        """Test case for v1_task_task_id_patch_post

        Submit Patch
        """
        payload = openapi_server.TypesPatchSubmission()
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/patch'.format(task_id='task_id_example'),
            method='POST',
            headers=headers,
            data=json.dumps(payload),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
