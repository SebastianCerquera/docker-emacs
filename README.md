#### Dockerized Emacs with GUI(Mac, Windows, GNU/Linux and your browser)

#### Tags:
 - `latest`  [dockerfiles/emacs26]()

#### Why
It [adds](https://github.com/JAremko/docker-emacs) some webservices that allow to do automation without having to connect to the emacs UI, it uses cask to do project management and ert to implement some integration tests.


##### GNU/Linux
```
sudo docker stop x11-bridge
sudo docker rm x11-bridge

sudo docker run -d \
 --name x11-bridge \
 -e MODE="tcp" \
 -e XPRA_HTML="yes" \
 -e DISPLAY=:14 \
 -e XPRA_PASSWORD=111 \
 --net=host \
 jare/x11-bridge

DATA_VOLUME=/data
EMACS_IMAGE=pandorasys/lain-emacs:1.0.5

sudo docker stop lain
sudo docker rm lain
sudo docker run -d --name lain \
 --volumes-from x11-bridge \
 -e DISPLAY=:14 -p 8000:8080 -v $DATA_VOLUME:$DATA_VOLUME \
$EMACS_IMAGE emacs
```

#### Test:

This assumes that cask is alarady installed:

cd lain
git checkout -- test/AGENDA/PROJECT.org

export PATH="~/.cask/bin:$PATH"
cask
./run-tests.sh