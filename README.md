### Emacs Docker with GUI support

(Clickable!)

[![http://i.imgur.com/rONW3c3.jpg](http://i.imgur.com/B1gvpmK.jpg)](http://i.imgur.com/rONW3c3.jpg)
[![http://i.imgur.com/mjl9ALQ.jpg](http://i.imgur.com/j6TO942.jpg)](http://i.imgur.com/mjl9ALQ.jpg)
[![http://i.imgur.com/RB46TA9.jpg](http://i.imgur.com/PCpbVg0.jpg)](http://i.imgur.com/RB46TA9.jpg)

### Why?
  - Reap the benefit of Emacs and other GNU/Linux tools on Windows/MacOS machine
  - Use https://hub.docker.com/ to auto-build your environment and store backups
  - Work with the same development environment everywhere
  - Run untrusted/risky code in the tunable sandbox with CPU/network/disk quotas if needed
  - Try new tools, perform destructive experiment and roll back changes when something goes wrong
  - Share your setup with others or extend someone else's builds
  - Run multiple Emacs instances on the same machine isolated
  - [Pause](https://docs.docker.com/engine/reference/commandline/pause) container to free resources temporarily
  - [Checkpoint & Restore](https://github.com/docker/docker/blob/1.13.x/experimental/checkpoint-restore.md) - potentially fastest way to start a "heavy" development environment

### How to use

First install [docker](https://docs.docker.com/engine/installation/)

#### MacOS:
Get [`XQuartz`](https://www.xquartz.org)

```
open -a XQuartz
```
In the XQuartz preferences, go to the “Security” tab and make sure you’ve got “Allow connections from network clients” ticked
```
ip=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
xhost + $ip
docker run -ti --name emacs\
 -e DISPLAY=$ip:0\
 -e UNAME="emacser"\
 -e GNAME="emacsers"\
 -e UID="1000"\
 -e GID="1000"\
 -v <path_to_your_.emacs.d>:/home/emacs/.emacs.d\
 -v <path_to_your_workspace>:/mnt/workspace\
 jare/emacs emacs
```
*[source](https://fredrikaverpil.github.io/2016/07/31/docker-for-mac-and-gui-applications/)*

*[other method](https://github.com/chanezon/docker-tips/blob/master/x11/README.md)*

#### Windows
Get [Cygwin](https://www.cygwin.com/) with `xinit`, `xorg-server` and optionaly [`winpty`](https://github.com/rprichard/winpty)(to run Emacs container with `-t`)
```
export DISPLAY=<your-machine-ip>:0.0
startxwin -- -listen tcp &
xhost + <your-machine-ip>
```

```
docker run --name emacs\
 -e DISPLAY="unix$DISPLAY"\
 -e UNAME="emacser"\
 -e GNAME="emacsers"\
 -e UID="1000"\
 -e GID="1000"\
 -v <path_to_your_.emacs.d>:/home/emacs/.emacs.d\
 -v <path_to_your_workspace>:/mnt/workspace\
 jare/emacs emacs
 ```
 Or with `-ti` via `winpty`
 ```
winpty docker run -ti --name emacs\
 -e DISPLAY="unix$DISPLAY"\
 -e UNAME="emacser"\
 -e GNAME="emacsers"\
 -e UID="1000"\
 -e GID="1000"\
 -v <path_to_your_.emacs.d>:/home/emacs/.emacs.d\
 -v <path_to_your_workspace>:/mnt/workspace\
 jare/emacs emacs
 ```
*[source](http://manomarks.github.io/2015/12/03/docker-gui-windows.html)*
*You can use [@ninrod 's vagrant setup](https://github.com/JAremko/docker-emacs/issues/2#issuecomment-260047233)*

#### GNU/Linux
*`UID` and preferably `UNAME` should match the host's user id.
Also make sure that `$DISPLAY` variable is set*
```
docker run -ti --name emacs -v /tmp/.X11-unix:/tmp/.X11-unix:ro\
 -e DISPLAY="unix$DISPLAY"\
 -e UNAME="emacser"\
 -e GNAME="emacsers"\
 -e UID="1000"\
 -e GID="1000"\
 -v <path_to_your_.emacs.d>:/home/emacs/.emacs.d\
 -v <path_to_your_workspace>:/mnt/workspace\
 jare/emacs emacs
```
That's it! Now you should see Emacs window.

##### If it doesn't work

You may need to allow local connection for the user
`UNAME` should match the hosts user id.
```
xhost +si:localuser:<UNAME>
```
Or allow local connection from the container's hostname(This should work with any `UID`)
```
xhost +local:`docker inspect --format='{{ .Config.Hostname }}' emacs`
```
*[source](http://stackoverflow.com/questions/25281992/alternatives-to-ssh-x11-forwarding-for-docker-containers)*

#### Also you can run it with  [docker-x11-bridge](https://github.com/JAremko/docker-x11-bridge)
##### Pros:
  - Same client for GNU/Linux, Windows and MacOS + web-browser
  - Persistent server (you can connect and disconnect without disrupting Emacs)
  - Interactive screen sharing [demo](https://imgur.com/ijdSuX6)
  - Read/write rss/email with Emacs in web-browser (O_O) [demo](https://imgur.com/wDLDMZN)

##### Cons:
  - Lag spikes with some OSes

#### Basic Docker commands:
  - `docker rm -f emacs` - remove `emacs` container
  - `docker restart emacs` - restart `emacs` container
  - `docker rmi -f jare/emacs` - remove `jare/emacs` image
  - `docker pull jare/emacs` - get newer `jare/emacs` version
  - `docker images -a` - list all images
  - `docker ps -a` - list all containers
  - `docker run ... jare/emacs` - run the [default command](https://github.com/JAremko/docker-emacs/blob/master/Dockerfile#L45)
  - `docker run -ti ... jare/emacs /bin/bash` - start bash
  - `docker exec emacs /usr/bin/emacs` - start `/usr/bin/emacs` in the running `emacs` container
  - `docker logs emacs` - print `emacs` container's logs
  - Read [Docker run reference](https://docs.docker.com/engine/reference/run/)
