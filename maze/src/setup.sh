function usage() {
  echo "$0 <rows> <colums> <seed>"
}

export rows=$1
export columns=$2
export seed=$3

if [ -z $rows ]
then
  usage
  exit 0
fi

if [ -z $columns ]
then
  usage
  exit 0
fi

if [ -z $seed ]
then
  usage
  exit 0
fi

if [ -e mazeIn ]
then
    rm mazeIn
fi

ghc -imaze -O2 -o MazeMain --make Main

mkfifo mazeIn

if [ -e mazeOut ]
then
    rm mazeOut
fi

mkfifo mazeOut

tail -f mazeIn | ./MazeMain $rows $columns $seed > mazeOut &

