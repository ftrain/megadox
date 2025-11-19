#!/bin/bash
cd nethack && rm -rf output && make epub html pdf && cd ..
cd postgresql && rm -rf output && make epub html pdf && cd ..
cd libsignal/encyclopedia && rm -rf output && make epub html pdf && cd ../..
cd unix-history-repo/docs && rm -rf output && make epub html pdf && cd ../..
cd surge && rm -rf output && make epub html pdf && cd ..
echo "All builds complete!"
