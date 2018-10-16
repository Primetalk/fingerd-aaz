#!/usr/bin/env bash
curl \
  --header "Content-Type: application/json" \
  --request POST \
  --data @vasya.json \
  http://localhost:8090/adduser/vasya
