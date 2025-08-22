import unittest

from flask import json

from openapi_server.models.types_ping_response import TypesPingResponse  # noqa: E501
from openapi_server.test import BaseTestCase


class TestPingController(BaseTestCase):
    """PingController integration test stubs"""

    def test_v1_ping_get(self):
        """Test case for v1_ping_get

        Test authentication creds and network connectivity
        """
        headers = { 
            'Accept': '*/*',
            'Authorization': 'Basic Zm9vOmJhcg==',
        }
        response = self.client.open(
            '/v1/ping/',
            method='GET',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
