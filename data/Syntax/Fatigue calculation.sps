* Encoding: UTF-8.
*FATIGUE SCORES (recode and calculate).

*recode fatigue individual scores for item 2,5,9,10,13,14,16,17,18,19. 
RECODE BFAT2 BFAT5 BFAT9 BFAT10 BFAT13 BFAT14 BFAT16 BFAT17 BFAT18 BFAT19 (1=5) (2=4) (3=3) (4=2) 
    (5=1) INTO fat2recoded fat5recoded fat9recoded fat10recoded fat13recoded fat14recoded fat16recoded 
    fat17recoded fat18recoded fat19recoded. 
VARIABLE LABELS  fat2recoded 'fat2recoded' /fat5recoded 'fat5recoded' /fat9recoded 'fat9recoded' 
    /fat10recoded 'fat10recoded' /fat13recoded 'fat13recoded' /fat14recoded 'fat14recoded' 
    /fat16recoded 'fat16recoded' /fat17recoded 'fat17recoded' /fat18recoded 'fat18recoded' 
    /fat19recoded 'fat19recoded'. 
EXECUTE.

FORMATS 
fat2recoded (f8.0)
fat5recoded (f8.0) 
fat9recoded (f8.0)
fat10recoded (f8.0)
fat13recoded (f8.0) 
fat14recoded (f8.0) 
fat16recoded (f8.0) 
fat17recoded (f8.0)
fat18recoded (f8.0) 
fat19recoded (f8.0) 
.

COMPUTE GeneralFatigueT0=BFAT1 + fat5recoded + BFAT12 + fat16recoded. 
EXECUTE. 
COMPUTE PhysicalFatigueT0=fat2recoded + BFAT8 + fat14recoded + BFAT20. 
EXECUTE. 
COMPUTE ReducedActivityT0=BFAT3 + BFAT6 + fat10recoded + fat17recoded. 
EXECUTE. 
COMPUTE ReducedMotivationT0=BFAT4 + fat9recoded + BFAT15 + fat18recoded. 
EXECUTE. 
COMPUTE MentalFatigueT0=BFAT7 + BFAT11 + fat13recoded + fat19recoded. 
EXECUTE.

FORMATS
GeneralFatigueT0 (f8.0)
PhysicalFatigueT0 (f8.0)
ReducedActivityT0 (f8.0)
ReducedMotivationT0 (f8.0)
MentalFatigueT0 (f8.0)
.
