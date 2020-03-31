#!/bin/bash

ulimit -d unlimited
ulimit -s unlimited
ulimit -v unlimited

#Add intel64 directory to library path for swan wave model
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/intel/lib/intel64
export OMP_STACKSIZE=65000

#http://javarevisited.blogspot.sg/2011/11/hotspot-jvm-options-java-examples.html
java -Xms5000m -Xmx8048m -XX:MaxPermSize=1024m -XX:+UseParallelGC -jar /ARMS/Workspaces/arms.jar
