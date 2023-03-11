#!/bin/bash

usage() {
	echo "Použití:"
	echo "start.sh [-h] [-i <image>] [-n <container name>] [-d <data folder>]"
	echo "  -h ... vypíše nápovědu"
	echo "  -i <image> ... volitelný název použitého docker image; implicitně smikula/trafficacc:latest"
	echo "  -n <container name> ... volitelný název kontejneru"
	echo "  -o <port number> ... číslo portu pro web server; implicitně 80"
    echo "  -p <docker run parameters> ... volitelné parametry předané dockeru při spuštění kontejneru"
    echo "                             ... např. -p \"--entrypoint bash\" = spustí shell místo www serveru a umožní inspekci kontejneru"
	echo "  -d <data folder> ... volitelná cesta ke složce s daty; implicitně aktuální adresář"
	exit 2
}

# implicit values
IMAGE="smikula/trafficacc:latest"
DATAFOLDER="$(pwd)"
CONTAINERNAME=""
CONTAINERARGS=""
DOCKERPARAMS=""
PORTNUM=80

# evaluate options
while getopts ":hi:n:d:o:p:" OPTION; do
	case $OPTION in
	    h)
	        usage
	        ;;
		i)
			IMAGE="$OPTARG"
			;;
		n)
		    CONTAINERNAME="--name $OPTARG"
		    ;;
		d)
		    DATAFOLDER="$OPTARG"
		    ;;
		o)
		    PORTNUM="$OPTARG"
		    ;;
		p)
		    DOCKERPARAMS="$OPTARG"
		    ;;
		m | c | k)
		    CONTAINERARGS="$CONTAINERARGS -$OPTION"
			;;
		:)
			echo "Chyba: Přepínač '$OPTARG' vyžaduje hodnotu."
			usage
			;;
		\?)
			echo "Chyba: Neznámý přepínač '$OPTARG'."
			usage
			;;
	esac
done
shift "$(($OPTIND -1))"

[ -z "$*" ] || usage

docker run -it --rm \
--mount type=bind,source="$DATAFOLDER/accidents",target=/srv/shiny-server/accidents --mount type=bind,source="$DATAFOLDER/clusters",target=/srv/shiny-server/clusters --mount type=bind,source="$DATAFOLDER/districts",target=/srv/shiny-server/districts -p $PORTNUM:3838 \
$CONTAINERNAME $DOCKERPARAMS $IMAGE $CONTAINERARGS
