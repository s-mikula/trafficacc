[CmdletBinding(PositionalBinding=$False)]
param(
    [Parameter()] [switch] $h,
    [Parameter()] [string] $i = "smikula/trafficacc:latest",
    [Parameter()] [string] $n = "",
    [Parameter()] [string] $d = "$(pwd)",
    [Parameter()] [string] $o = 80,
    [Parameter()] [string] $p = ""
#    [Parameter(ValueFromRemainingArguments)] [string] $Remaining
)

function usage {
	echo "Použití:"
	echo "start.sh [-h] [-i <image>] [-n <container name>] [-d <data folder>]"
	echo "  -h ... vypíše nápovědu"
	echo "  -i <image> ... volitelný název použitého docker image; implicitně smikula/trafficacc:latest"
	echo "  -n <container name> ... volitelný název kontejneru"
	echo "  -o <port number> ... číslo portu pro web server; implicitně 80"
    echo "  -p <docker run parameters> ... volitelné parametry předané dockeru při spuštění kontejneru"
    echo "                             ... např. -p '--entrypoint bash' = spustí shell místo www serveru a umožní inspekci kontejneru"
	echo "  -d <data folder> ... volitelná cesta ke složce s daty; implicitně aktuální adresář"
	exit 2
}

if ($h) { usage }
if ($n) { $n = "--name $n" }

#Set-PSDebug -Trace 1

$cmd="docker run -it --rm --mount type=bind,source='$d\accidents',target=/srv/shiny-server/accidents --mount type=bind,source='$d\clusters',target=/srv/shiny-server/clusters --mount type=bind,source='$d\districts',target=/srv/shiny-server/districts -p ${o}:3838 $n $p $i"
Invoke-Expression $cmd
