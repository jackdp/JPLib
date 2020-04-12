@echo off

set Template=JPLib_TEMPLATE.dpk
set Desc="JPLib Runtime Package"
set BaseOutDir=..\Delphi
set ShortDpkName=JPLib.dpk

for %%x in (2009,2010,XE,XE2,XE3,XE4,XE5,XE6,XE7,XE8) do (
  DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_%%x\%ShortDpkName% -s %%x
)

DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_10.0_Seattle\%ShortDpkName% -s Seattle
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_10.1_Berlin\%ShortDpkName% -s Berlin
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_10.2_Tokyo\%ShortDpkName% -s Tokyo
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_10.3_Rio\%ShortDpkName% -s Rio


StrRep.exe "..\Delphi_2009\%ShortDpkName%" "JPL.LangMgr" "//JPL.LangMgr" 0 true first
StrRep.exe "..\Delphi_2009\%ShortDpkName%" "JPL.RTTI" "//JPL.RTTI" 0 true first

