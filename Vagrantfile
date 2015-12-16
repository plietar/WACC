Vagrant.configure(2) do |config|
  config.vm.box = "debian/jessie64"
  config.vm.box_version = "8.2.1"
  config.vm.provision "shell", inline: <<-SHELL
    set -ex
    cat > /etc/apt/sources.list.d/emdebian.list <<EOF
deb http://emdebian.org/tools/debian/ jessie main
EOF
    curl http://emdebian.org/tools/debian/emdebian-toolchain-archive.key | sudo apt-key add -

    sudo dpkg --add-architecture armel
    sudo apt-get update
    sudo apt-get install -y qemu haskell-platform make crossbuild-essential-armel
    sudo apt-get install -y libc6-i386 lib32stdc++6 lib32z1 lib32ncurses5 lib32ncursesw5
    cabal update
    cabal install fgl optparse-applicative


    wget -q -r -O /tmp/gcc-linaro-arm-linux-gnueabihf-4.8-2013.10_linux.tar.bz2 \
      https://launchpad.net/linaro-toolchain-binaries/trunk/2013.10/+download/gcc-linaro-arm-linux-gnueabihf-4.8-2013.10_linux.tar.bz2

    sudo tar -C /opt -xf /tmp/gcc-linaro-arm-linux-gnueabihf-4.8-2013.10_linux.tar.bz2 \
        gcc-linaro-arm-linux-gnueabihf-4.8-2013.10_linux/bin/arm-linux-gnueabihf-gdb

    cat > /etc/profile.d/gcc-linaro.sh <<EOF
export PATH=/opt/gcc-linaro-arm-linux-gnueabihf-4.8-2013.10_linux/bin:$PATH
EOF

  SHELL
end
