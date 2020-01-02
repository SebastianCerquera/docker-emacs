// it is still missing to install emacs and cask to the agent
// apt-get update && apt-get install -y emacs
// curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

node("runner") {
  sh("ssh-keyscan -t rsa git-container >> ~/.ssh/known_hosts")
  checkout([ 
       $class: 'GitSCM',
       branches: [[
	   name: 'master'
       ]],
       doGenerateSubmoduleConfigurations: false,
       submoduleCfg: [],
       userRemoteConfigs: [[
	   url: 'git@git-container:/home/git/sources/docker-emacs.git',
	   credentialsId: 'git'
       ]]
   ]);
  dir("lain") {
     sh("mkdir -p /tmp/org/")
     sh("~/.cask/bin/cask")
     sh("./run-tests.sh")
  }
}