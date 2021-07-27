#!/usr/bin/env bash

read -p 'MFA Token: ' otp

line=$(aws sts get-session-token --profile user --serial-number $AWS_MFA_SERIAL --token-code $otp --duration-seconds 129600 --output text)
echo $line | awk 'BEGIN {FS=" "}; {printf("[default]\naws_access_key_id=%s\naws_secret_access_key=%s\naws_session_token=%s\n#expiry %s",$2,$4,$5,$3)}' > ~/.aws/credentials
