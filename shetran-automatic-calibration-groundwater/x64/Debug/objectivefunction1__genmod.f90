        !COMPILER-GENERATED INTERFACE MODULE: Fri Mar  8 16:09:23 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OBJECTIVEFUNCTION1__genmod
          INTERFACE 
            SUBROUTINE OBJECTIVEFUNCTION1(NSE,BIAS,KGE,LOGNSE,RMSE,PEA, &
     &SPE,NSEFRAC,BIASFRAC,KGEFRAC,LOGNSEFRAC,RMSEFRAC,PEAFRAC,SPEFRAC, &
     &OBJFN)
              REAL(KIND=4), INTENT(IN) :: NSE
              REAL(KIND=4), INTENT(IN) :: BIAS
              REAL(KIND=4), INTENT(IN) :: KGE
              REAL(KIND=4), INTENT(IN) :: LOGNSE
              REAL(KIND=4), INTENT(IN) :: RMSE
              REAL(KIND=4), INTENT(IN) :: PEA
              REAL(KIND=4), INTENT(IN) :: SPE
              REAL(KIND=4), INTENT(IN) :: NSEFRAC
              REAL(KIND=4), INTENT(IN) :: BIASFRAC
              REAL(KIND=4), INTENT(IN) :: KGEFRAC
              REAL(KIND=4), INTENT(IN) :: LOGNSEFRAC
              REAL(KIND=4), INTENT(IN) :: RMSEFRAC
              REAL(KIND=4), INTENT(IN) :: PEAFRAC
              REAL(KIND=4), INTENT(IN) :: SPEFRAC
              REAL(KIND=4), INTENT(OUT) :: OBJFN
            END SUBROUTINE OBJECTIVEFUNCTION1
          END INTERFACE 
        END MODULE OBJECTIVEFUNCTION1__genmod
