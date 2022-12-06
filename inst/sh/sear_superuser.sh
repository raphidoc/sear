#!/bin/bash
# Ask user for sudo password (to be used when needed)
read -s -p "Enter Password for sudo: " sudoPW

#echo $sudoPW | sudo systemctl stop shiny-server

if grep -q searuser /etc/group
    then
         echo "group supersear exist"
    else
         echo $sudoPW | sudo groupadd -g 666 supersear
         echo "group supersear created"
    fi

if grep -q searuser /etc/group
    then
         echo "group searuser exist"
    else
         echo $sudoPW | sudo groupadd -g 665 searuser
         echo "group searuser created"
    fi

# Could add a custom -k, --skel SKEL_DIR with the -m (--create-home) option
# specific home location with d (--home) option

echo $sudoPW | sudo useradd -m -g sudo -G supersear,searuser -s /bin/bash sear 
echo "sear:$sudoPW" | sudo chpasswd

sudo -i -u sear bash << EOF
echo "In"

whoami
pwd

mkdir .ssh

# Add ssh key for pop
echo 'ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDT15r/QchiC8UxrThkfm4FRLRBRxhdoxyqA6SifarE0pvTsgxvngrzHajWxHuVmE3D3tI9pN6+NLDprJtXZ5FhBV+cqkia1edmC0bHo4boyEVPQ3KcnXUKDXGZrQGrDqzvsuzN3WHHhiph7mUkrelEDGpkgiWrFmTyBtIP5KgAoV3k8n6VPD5vChsQ0/CVkiezh2GkaPoZRkvGNox7RNmsciBRCgbjS2UrUndMX0ZJVBNvT4jklu9tytXRC3RxmhLSLWhBrWuUeXjLVWnUw0e/nebii8DUA3VEgGtcgRGvKfstq9dqZ5R6hpomLBCtuU/P7R9wyPYwQdLUDaFVYnHF/99MH5BBtNVpqLACn7yRHx193Q5JfWcSvUsnj/8nF4yS2o/yq5URFKemKlXdFex2XvsOw6CTWPYNeUi4x3T4NzVbCzgkfpwI8lgp4Ax/To9C3yJTmmCHzHYE9Vq9f3usivWegH7nRYIelC1LEM9mVSJnSnjNhny2SX4P6PwIuvs= raphael@pop-os' > .ssh/authorized_keys

# and GitHub
echo '-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACC2ADujk5wZCneZWGVylaY5ZvhCBuflVS4JVuHBeopVQQAAAKBoT4bXaE+G
1wAAAAtzc2gtZWQyNTUxOQAAACC2ADujk5wZCneZWGVylaY5ZvhCBuflVS4JVuHBeopVQQ
AAAEBqy9JWdBySW9j8/E4qg+oqMSIfOfHgNzrbzpRlZ0kwObYAO6OTnBkKd5lYZXKVpjlm
+EIG5+VVLglW4cF6ilVBAAAAF3JhcGhhZWwubWFiaXRAZ21haWwuY29tAQIDBAUG
-----END OPENSSH PRIVATE KEY-----
' > .ssh/id_ed25519

chmod 400 .ssh/id_ed25519

# Clone sear
mkdir ShinyApps
cd ShinyApps
git clone git@github.com:raphidoc/sear.git
cd sear
git fetch
git checkout demo

echo "Out"
EOF

echo $sudoPW | sudo systemctl restart shiny-server
echo $sudoPW | sudo systemctl status shiny-server

