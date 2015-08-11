---
title: Oracle, VirtualBox, Docker and Mac OS X
date: 2014-02-01T15:14:00
slug: oracle-virtualbox-docker-osx
categories: [ "docker", "osx" ]
---

You might have heard of [Docker](http://www.docker.io "Docker") by now. It's a young but ambitious project that promises to package up
your application as a portable and self-sufficient container ready to run in any environment. I already use Docker on my Linux boxes to
spin several test instances of my web service. Let's see if we can run it on Mac OS X.

<!--more-->

The web service is a SOAP server that uses Oracle database to persist data. Users interact with the service via a web interface.
The web service and the web interface are packaged as a container that exposes TCP port 80. So we need to set up a Docker server and
an Oracle database instance. There isn't native Docker server for OS X yet so we will use
[VirtualBox](https://www.virtualbox.org/wiki/Downloads "VirualBox") to run them.

The easiest way to run an Oracle database on OS X is to use a [preconfigured virtual machine](http://www.oracle.com/technetwork/database/enterprise-edition/databaseappdev-vm-161299.html "Oracle VM for developers").
Install VirtualBox and import the machine.

The next step is to install tiny [boot2docker](https://github.com/steeve/boot2docker "boot2docker") VM that runs a Docker server:

```bash
mkdir boot2docker && cd boot2docker
curl https://raw.github.com/steeve/boot2docker/master/boot2docker > boot2docker
chmod +x boot2docker
./boot2docker init
./boot2docker up
./boot2docker ssh
```

The last command logs us into the virtual machine. boot2docker doesn't persist containers by default but we can change this:

```bash
    $ su -
    $ fdisk /dev/sda
    n
    p
    1

    w
    $ mkfs.ext4 /dev/sda1
    $ exit
```

Now restart the boot2docker VM:

```bash
./boot2docker restart
```

At this point we can pull and run docker containers but there are a few problems:

1. boot2docker doesn't expose container ports to the host machine;
2. docker containers can't access the Oracle DB.

The first problem is easy. Open VirtualBox GUI, stop the boot2docker VM and set up it to forward ports to their host targets.

For the other one we will have to add a new network interface to the boot2docker VM and to the Oracle VM. Set up
both to use internal network mode. We will also have to configure VirtualBox DHCP server. Here is an example how to do it:

```bash
VBoxManage dhcpserver add
    --netname intnet
    --ip 10.0.3.1
    --netmask 255.255.255.0
    --lowerip 10.0.3.2
    --upperip 10.0.3.100
    --enable
```

We add a DHCP server for internal network called 'intnet' with 10.0.3.1 as the gateway IP. The DHCP server assigns
clients addresses starting from 10.0.3.2 to 10.0.3.100.

The boot2docker VM can use a dynamic IP in the internal network. The Oracle VM ought to have a static address so
boot the VM and use the ```system-config-network-tui``` utility to assign a static IP (for example, 10.0.3.101) to ```eth0``` network interface.

Now applications in our containers can connect to the database:

```bash
docker run
    -d
    -p 8080:80
    -e DB_HOST=10.0.3.101
    -e DB_USER=example
    -e DB_PASSWORD=example
    example-app-container
```

With port forwarding set up to forward TCP port 8080 to host port 8080 we can access the web app using this URL:

    http://localhost:8080
