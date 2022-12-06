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

