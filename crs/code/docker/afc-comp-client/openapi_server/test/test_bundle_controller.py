import unittest

from flask import json

from openapi_server.models.types_bundle_submission import TypesBundleSubmission  # noqa: E501
from openapi_server.models.types_bundle_submission_response import TypesBundleSubmissionResponse  # noqa: E501
from openapi_server.models.types_bundle_submission_response_verbose import TypesBundleSubmissionResponseVerbose  # noqa: E501
from openapi_server.models.types_error import TypesError  # noqa: E501
from openapi_server.test import BaseTestCase


class TestBundleController(BaseTestCase):
    """BundleController integration test stubs"""

    def test_v1_task_task_id_bundle_bundle_id_delete(self):
        """Test case for v1_task_task_id_bundle_bundle_id_delete

        Delete Bundle
        """
        headers = { 
            'Accept': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/bundle/{bundle_id}'.format(task_id='task_id_example', bundle_id='bundle_id_example'),
            method='DELETE',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_v1_task_task_id_bundle_bundle_id_get(self):
        """Test case for v1_task_task_id_bundle_bundle_id_get

        Get Bundle
        """
        headers = { 
            'Accept': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/bundle/{bundle_id}'.format(task_id='task_id_example', bundle_id='bundle_id_example'),
            method='GET',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_v1_task_task_id_bundle_bundle_id_patch(self):
        """Test case for v1_task_task_id_bundle_bundle_id_patch

        Update Bundle
        """
        payload = openapi_server.TypesBundleSubmission()
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/bundle/{bundle_id}'.format(task_id='task_id_example', bundle_id='bundle_id_example'),
            method='PATCH',
            headers=headers,
            data=json.dumps(payload),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_v1_task_task_id_bundle_post(self):
        """Test case for v1_task_task_id_bundle_post

        Submit Bundle
        """
        payload = openapi_server.TypesBundleSubmission()
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/task/{task_id}/bundle'.format(task_id='task_id_example'),
            method='POST',
            headers=headers,
            data=json.dumps(payload),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
