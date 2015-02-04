'' Basic I/O Test
'' Sets all outputs to high to verify that we can actually output values
''

PUB start
  dira := $FFFF_FFFF            '' Set to output
  outa := $FFFF_FFFF            '' Set pins to high

  repeat while true             '' keep COG alive