#!/usr/bin/env python3

import os
import openai
import anthropic
import google.generativeai as genai
import requests

def check_openai():
  try:
    openai_client = openai.OpenAI(api_key=os.environ.get("OPENAI_API_KEY"))
    if not openai_client.api_key:
      return "OPENAI_API_KEY not set!"
    response = openai_client.chat.completions.create(
      model="gpt-4o",
      messages=[
        {
          "role": "user",
          "content":"Generate Lorem Ipsum."
        }
      ]
    )
    # print(f"\n{response}")
    return "OpenAI Success!"
  except Exception as e:
    return f"OpenAI: Failed! - {e}"

def check_anthropic():
  try:
    anthro_client = anthropic.Anthropic(api_key=os.environ.get("ANTHROPIC_API_KEY"))
    if not anthro_client.api_key:
      return "ANTHROPIC_API_KEY not set!"
    response = anthro_client.messages.create(
      model="claude-3-5-sonnet-20240620",
      max_tokens=10,
      messages=[
        {
          "role": "user",
          "content":"Generate Lorem Ipsum."
        }
      ]
    )
    # print(f"\n{response}")
    return "Anthropic Success!"
  except Exception as e:
    return f"Anthropic Failed! - {e}"

def check_gemini():
  try:
    genai.configure(api_key=os.environ.get("GEMINI_API_KEY"))
    model = genai.GenerativeModel("gemini-2.0-flash")
    response = model.generate_content("Generate Lorem Ipsum.")
    # print(f"\n{response}")
    return "Gemini Success!"
  except Exception as e:
    return f"Gemini: Failed! - {e}"

if __name__ == "__main__":
  print(check_openai())
  print(check_anthropic())
  print(check_gemini())
