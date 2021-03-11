#ifndef BMI_HYDROTREND_H_INCLUDED
#define BMI_HYDROTREND_H_INCLUDED

#if defined(__cplusplus)
extern "C" {
#endif

#define HT_MAJOR_VERSION (3)
#define HT_MINOR_VERSION (0)
#define HT_MICRO_VERSION (5)

#include <bmi.h>

Bmi * register_bmi_hydrotrend(Bmi *model);

#if defined(__cplusplus)
}
#endif

#endif
