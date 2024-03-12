        !COMPILER-GENERATED INTERFACE MODULE: Fri Mar  8 16:09:23 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE JULIAN_DATE__genmod
          INTERFACE 
            FUNCTION JULIAN_DATE(YYYY,MM,DD) RESULT(JULIAN)
              INTEGER(KIND=4), INTENT(IN) :: YYYY
              INTEGER(KIND=4), INTENT(IN) :: MM
              INTEGER(KIND=4), INTENT(IN) :: DD
              INTEGER(KIND=4) :: JULIAN
            END FUNCTION JULIAN_DATE
          END INTERFACE 
        END MODULE JULIAN_DATE__genmod
