# This script is responsible for retreiving battery
# info, requires acpi

exists=$(which acpi 2> /dev/null)
if [ $exists ] ; then

    echo -ne "power\t"

    acpi -b | cut -d ' ' -f 4 | tr '\n' ' ' | sed 's/[,%]//g'
    AC=$(acpi -a | cut -d ' ' -f 3)

    case $AC in
        off-line)
            echo -n "O";;
        on-line)
            echo -n "C";;
        *)
            echo -n "?";;
    esac
    echo

fi
