        !COMPILER-GENERATED INTERFACE MODULE: Fri Mar  8 16:09:22 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OBJECTIVEFUNCTION__genmod
          INTERFACE 
            SUBROUTINE OBJECTIVEFUNCTION(NVALUES,NSE,BIAS,KGE,LOGNSE,   &
     &RMSE,PEA,SPE,NSEFRAC,BIASFRAC,KGEFRAC,LOGNSEFRAC,RMSEFRAC,PEAFRAC,&
     &SPEFRAC,OBJFN)
              INTEGER(KIND=4), INTENT(IN) :: NVALUES
              REAL(KIND=4), INTENT(IN) :: NSE(NVALUES)
              REAL(KIND=4), INTENT(IN) :: BIAS(NVALUES)
              REAL(KIND=4), INTENT(IN) :: KGE(NVALUES)
              REAL(KIND=4), INTENT(IN) :: LOGNSE(NVALUES)
              REAL(KIND=4), INTENT(IN) :: RMSE(NVALUES)
              REAL(KIND=4), INTENT(IN) :: PEA(NVALUES)
              REAL(KIND=4), INTENT(IN) :: SPE(NVALUES)
              REAL(KIND=4), INTENT(IN) :: NSEFRAC
              REAL(KIND=4), INTENT(IN) :: BIASFRAC
              REAL(KIND=4), INTENT(IN) :: KGEFRAC
              REAL(KIND=4), INTENT(IN) :: LOGNSEFRAC
              REAL(KIND=4), INTENT(IN) :: RMSEFRAC
              REAL(KIND=4), INTENT(IN) :: PEAFRAC
              REAL(KIND=4), INTENT(IN) :: SPEFRAC
              REAL(KIND=4), INTENT(OUT) :: OBJFN(NVALUES)
            END SUBROUTINE OBJECTIVEFUNCTION
          END INTERFACE 
        END MODULE OBJECTIVEFUNCTION__genmod
