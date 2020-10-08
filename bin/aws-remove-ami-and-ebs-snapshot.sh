#! /bin/bash

region=$1
amiId=$2

runEc2() {
  /usr/bin/aws ec2 --region $region $@
}

snapId=$(runEc2 describe-images --image-id $amiId | jq -r -M '.Images[0].BlockDeviceMappings[0].Ebs.SnapshotId')

runEc2 deregister-image --image-id $amiId
runEc2 delete-snapshot --snapshot-id $snapId
