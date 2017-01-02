spinner()
{
    local pid=$!
    local delay=0.5
    spin='—\|/'
    while kill -0 $pid 2>/dev/null
    do
        i=$(( (i+1)%4 ))
        printf "\r* booting file...${spin:$i:1}"
        sleep $delay
    done

}

$@ 2>/dev/null & spinner
