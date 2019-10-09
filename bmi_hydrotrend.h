#ifndef BMI_HYDROTREND_H_INCLUDED
#define BMI_HYDROTREND_H_INCLUDED

#if defined(__cplusplus)
extern "C" {
#endif

#define HT_MAJOR_VERSION (3)
#define HT_MINOR_VERSION (0)
#define HT_MICRO_VERSION (7)

#include "bmi.h"

BMI_Model * register_bmi_hydrotrend(BMI_Model *model);

#if defined(__cplusplus)
}
#endif

#endif
