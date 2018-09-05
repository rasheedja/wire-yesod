# Wire Yesod
A website insipred by twitter made using Yesod. This website was made for a comparison between Yesod and Django. A write-up of this comparison can be found [here](https://github.com/rasheedja/YesodDjangoComparison).

## Prerequisites
1. Vagrant >= 2.0 https://www.vagrantup.com/downloads.html
2. A version of VirtualBox supported by your version of vagrant. For vagrant 2.0, use VirtualBox 5.1 https://www.virtualbox.org/wiki/Download_Old_Builds_5_1

## Getting the site running
Install the vagrant-vbguest plugin `vagrant plugin install vagrant-vbguest`
`cd vagrant` and `vagrant up`
If this is your first `vagrant up`, go make a cup of tea. This will take a while
Once the `vagrant up` has finished, `vagrant ssh` into your box
`cd /yesod/wire`
`stack exec -- yesod devel`
Now you should be able to access the site at `localhost:3030`

