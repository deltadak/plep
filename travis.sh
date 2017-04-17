#!/bin/bash

# from https://github.com/Sorien/silex-idea-plugin

ideaVersion="2017.1"

if [ ! -d ./idea ]; then
    # Get our IDEA dependency
    wget http://download.jetbrains.com/idea/ideaIU-${ideaVersion}.tar.gz

    # Unzip IDEA
    tar zxf ideaIU-${ideaVersion}.tar.gz
    rm -rf ideaIU-${ideaVersion}.tar.gz

    # Move the versioned IDEA folder to a known location
    ideaPath=$(find . -name 'idea-IU*' | head -n 1)
    mv ${ideaPath} ./idea
fi

# Run the tests
if [ "$1" = "-d" ]; then
    ant -d -f build.xml -DIDEA_HOME=./idea -DJAVE_HOME=/usr/lib/jvm/java-8-oracle
else
    ant -f build.xml -DIDEA_HOME=./idea -DJAVA_HOME=/usr/lib/jvm/java-8-oracle
fi

# Was our build successful?
stat=$?

if [ "${TRAVIS}" != true ]; then
    ant -f build.xml -q clean

    if [ "$1" = "-r" ]; then
        rm -rf idea
        rm -rf plugins
    fi
fi

# Return the build status
exit ${stat}