#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <ctype.h>

#include "bmi.h"
#include "hydrotrend_irf.h"

/* Implement this: Add model-specific includes */

static char *
_str_strip(char *str) {
  char * end;

  while (isspace(*str))
    str++;

  end = str + strlen(str) - 1;

  while (end > str && isspace(*end))
    end--;

  *(end + 1) = '\0';

  return str;
}



static int
get_component_name (BMI_Model *self, char * name)
{
    strncpy (name, "hydrotrend", BMI_MAX_COMPONENT_NAME);
    return BMI_SUCCESS;
}


#define INPUT_VAR_NAME_COUNT (0)
static const char *input_var_names[INPUT_VAR_NAME_COUNT] = {

};


static int
get_input_var_name_count(BMI_Model *self, int *count)
{
    *count = INPUT_VAR_NAME_COUNT;
    return BMI_SUCCESS;
}


static int
get_input_var_names(BMI_Model *self, char **names)
{
    int i;
    for (i=0; i<INPUT_VAR_NAME_COUNT; i++) {
        strncpy(names[i], input_var_names[i], BMI_MAX_VAR_NAME);
    }
    return BMI_SUCCESS;
}


#define OUTPUT_VAR_NAME_COUNT (11)
static const char *output_var_names[OUTPUT_VAR_NAME_COUNT] = {
    "atmosphere_bottom_air__domain_mean_of_temperature",
    "channel_exit_water_sediment~suspended__mass_flow_rate",
    "channel_exit_water_flow__speed",
    "channel_entrance_water_sediment~bedload__mass_flow_rate", // Repeat. Maybe remove.
    "channel_exit_water__volume_flow_rate",
    "channel_exit_water_x-section__width",
    "channel_exit_water_x-section__depth",
    "channel_entrance_water__volume_flow_rate", // Repeat. Maybe remove.
    "atmosphere_water__domain_mean_of_precipitation_leq-volume_flux",
    "channel_exit_water_sediment~bedload__mass_flow_rate",
    "channel_exit_water_sediment~suspended__mass_concentration"
};


static int
get_output_var_name_count(BMI_Model *self, int *count)
{
    *count = OUTPUT_VAR_NAME_COUNT;
    return BMI_SUCCESS;
}


static int
get_output_var_names(BMI_Model *self, char **names)
{
    int i;
    for (i=0; i<OUTPUT_VAR_NAME_COUNT; i++) {
        strncpy(names[i], output_var_names[i], BMI_MAX_VAR_NAME);
    }
    return BMI_SUCCESS;
}


static int
get_start_time(BMI_Model * self, double *time)
{
    *time = 0;
    return BMI_SUCCESS;
}


static int
get_end_time(BMI_Model * self, double *time)
{ /* Implement this: Set end time */
    *time  = ((HydrotrendData*)self->data)->n_days;
    return BMI_SUCCESS;
}


static int
get_current_time(BMI_Model * self, double *time)
{ /* Implement this: Set current time */
    *time = ((HydrotrendData*)self->data)->day;
    return BMI_SUCCESS;
}


static int
get_time_step(BMI_Model * self, double *dt)
{ /* Implement this: Set time step */
    *dt = 1.;
    return BMI_SUCCESS;
}


static int
get_time_units(BMI_Model * self, char *units)
{
    strncpy(units, "d", BMI_MAX_UNITS_NAME);
    return BMI_SUCCESS;
}


static int
initialize(BMI_Model *self, const char * file)
{ /* Implement this: Create and initialize a model handle */
  if (self) {
    char *in_dir = NULL;
    char *prefix = NULL;
    char *out_dir = NULL;

    if (file && file[0] != '\0') {
      FILE * fp = fopen (file, "r");

      if (fp) {
        char args[2048];

        if (fgets (args, 2048, fp)==args) {
          in_dir = _str_strip (strdup (args));
        }
        if (fgets (args, 2048, fp)==args) {
          prefix = _str_strip (strdup (args));
        }
        if (fgets (args, 2048, fp)==args) {
          out_dir = _str_strip (strdup (args));
        }
        fclose(fp);
      }
      else
          return BMI_FAILURE;
    }
    else {
      in_dir = strdup (".");
      prefix = strdup ("HYDRO");
      out_dir = strdup (".");
    }

    if (in_dir && prefix && out_dir) {
      hydro_initialize((HydrotrendData*)self->data, in_dir, prefix, out_dir);
    }
  }
  else
    return BMI_FAILURE;

  return BMI_SUCCESS;
}


static int
update_frac(BMI_Model * self, double f)
{ /* Implement this: Update for a fraction of a time step */
    return BMI_FAILURE;
}


static int
update(BMI_Model * self)
{
    double day;

    if (self->get_current_time(self, &day) == BMI_FAILURE)
        return BMI_FAILURE;

    hydro_run((HydrotrendData*)self->data, day + 1.);

    return BMI_SUCCESS;
}


static int
update_until(BMI_Model * self, double then)
{
    double dt;
    double now;

    if (self->get_time_step(self, &dt) == BMI_FAILURE)
        return BMI_FAILURE;

    if (self->get_current_time(self, &now) == BMI_FAILURE)
        return BMI_FAILURE;

    {
        int n;
        const double n_steps = (then - now) / dt;
        for (n=0; n<(int)n_steps; n++) {
            if (self->update(self) == BMI_FAILURE)
                return BMI_FAILURE;
        }
/*
        if (update_frac(self, n_steps - (int)n_steps) == BMI_FAILURE)
            return BMI_FAILURE;
*/
    }

    return BMI_SUCCESS;
}


static int
finalize(BMI_Model * self)
{ /* Implement this: Clean up */
    hydro_finalize ((HydrotrendData*)self->data);
    return BMI_SUCCESS;
}


static int
get_var_grid(BMI_Model *self, const char *name, int *grid)
{
    return BMI_FAILURE;
    if (strcmp(name, "atmosphere_bottom_air__domain_mean_of_temperature") == 0) {
        *grid = 0;
    } else if (strcmp(name, "channel_exit_water_sediment~suspended__mass_flow_rate") == 0) {
        *grid = 0;
    } else if (strcmp(name,"channel_exit_water_sediment~suspended__mass_concentration") == 0) {
        *grid = 0;
    } else if (strcmp(name, "channel_exit_water_flow__speed") == 0) {
        *grid = 0;
    } else if (strcmp(name, "channel_entrance_water_sediment~bedload__mass_flow_rate") == 0) {
        *grid = 0;
    } else if (strcmp(name, "channel_exit_water__volume_flow_rate") == 0) {
        *grid = 0;
    } else if (strcmp(name, "channel_exit_water_x-section__width") == 0) {
        *grid = 0;
    } else if (strcmp(name, "channel_exit_water_x-section__depth") == 0) {
        *grid = 0;
    } else if (strcmp(name, "channel_entrance_water__volume_flow_rate") == 0) {
        *grid = 0;
    } else if (strcmp(name, "atmosphere_water__domain_mean_of_precipitation_leq-volume_flux") == 0) {
        *grid = 0;
    } else if (strcmp(name, "channel_exit_water_sediment~bedload__mass_flow_rate") == 0) {
        *grid = 0;
    } else {
        *grid = -1; return BMI_FAILURE;
    }
    return BMI_SUCCESS;
}


static int
get_var_type(BMI_Model *self, const char *name, char *type)
{
    if (strcmp(name, "atmosphere_bottom_air__domain_mean_of_temperature") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_sediment~suspended__mass_flow_rate") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name,"channel_exit_water_sediment~suspended__mass_concentration") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_flow__speed") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_entrance_water_sediment~bedload__mass_flow_rate") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water__volume_flow_rate") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_x-section__width") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_x-section__depth") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_entrance_water__volume_flow_rate") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "atmosphere_water__domain_mean_of_precipitation_leq-volume_flux") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_sediment~bedload__mass_flow_rate") == 0) {
        strncpy(type, "double", BMI_MAX_UNITS_NAME);
    } else {
        type[0] = '\0'; return BMI_FAILURE;
    }
    return BMI_SUCCESS;
}


static int
get_var_units(BMI_Model *self, const char *name, char *units)
{
    if (strcmp(name, "atmosphere_bottom_air__domain_mean_of_temperature") == 0) {
        strncpy(units, "degree_Celsius", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_sediment~suspended__mass_flow_rate") == 0) {
        strncpy(units, "kg / s", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name,"channel_exit_water_sediment~suspended__mass_concentration") == 0) {
        strncpy(units, "kg / m^3", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_flow__speed") == 0) {
        strncpy(units, "m / s", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_entrance_water_sediment~bedload__mass_flow_rate") == 0) {
        strncpy(units, "kg / s", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water__volume_flow_rate") == 0) {
        strncpy(units, "m^3 / s", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_x-section__width") == 0) {
        strncpy(units, "m", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_x-section__depth") == 0) {
        strncpy(units, "m", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_entrance_water__volume_flow_rate") == 0) {
        strncpy(units, "m^3 / s", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "atmosphere_water__domain_mean_of_precipitation_leq-volume_flux") == 0) {
        strncpy(units, "m / d", BMI_MAX_UNITS_NAME);
    } else if (strcmp(name, "channel_exit_water_sediment~bedload__mass_flow_rate") == 0) {
        strncpy(units, "kg / s", BMI_MAX_UNITS_NAME);
    } else {
        units[0] = '\0'; return BMI_FAILURE;
    }
    return BMI_SUCCESS;
}


static int
get_var_itemsize(BMI_Model *self, const char *name, int *itemsize)
{
    if (strcmp(name, "atmosphere_bottom_air__domain_mean_of_temperature") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name, "channel_exit_water_sediment~suspended__mass_flow_rate") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name,"channel_exit_water_sediment~suspended__mass_concentration") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name, "channel_exit_water_flow__speed") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name, "channel_entrance_water_sediment~bedload__mass_flow_rate") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name, "channel_exit_water__volume_flow_rate") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name, "channel_exit_water_x-section__width") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name, "channel_exit_water_x-section__depth") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name, "channel_entrance_water__volume_flow_rate") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name, "atmosphere_water__domain_mean_of_precipitation_leq-volume_flux") == 0) {
        *itemsize = sizeof(double);
    } else if (strcmp(name, "channel_exit_water_sediment~bedload__mass_flow_rate") == 0) {
        *itemsize = sizeof(double);
    } else {
        *itemsize = 0; return BMI_FAILURE;
    }
    return BMI_SUCCESS;
}


static int
get_var_nbytes(BMI_Model *self, const char *name, int *nbytes)
{
    int itemsize;

    if (self->get_var_itemsize(self, name, &itemsize) == BMI_FAILURE)
        return BMI_FAILURE;

    *nbytes = itemsize;

    return BMI_SUCCESS;
}


static int
get_var_location(BMI_Model *self, const char *name, char *loc)
{
    strncpy(loc, "none", BMI_MAX_VAR_NAME);
    return BMI_SUCCESS;
}


static int
get_value_ptr(BMI_Model *self, const char *name, void **dest)
{
    if (strcmp(name, "atmosphere_bottom_air__domain_mean_of_temperature") == 0) {
        *dest = hydro_get_temperature_ptr(self->data);
    } else if (strcmp(name, "channel_exit_water_sediment~suspended__mass_flow_rate") == 0) {
        *dest = hydro_get_sediment_discharge_ptr(self->data);
    } else if (strcmp(name,"channel_exit_water_sediment~suspended__mass_concentration") == 0) {
        *dest = hydro_get_sediment_concentration_ptr(self->data);
    } else if (strcmp(name, "channel_exit_water_flow__speed") == 0) {
        *dest = hydro_get_velocity_ptr(self->data);
    } else if (strcmp(name, "channel_entrance_water_sediment~bedload__mass_flow_rate") == 0) {
        *dest = hydro_get_bedload_flux_ptr(self->data);
    } else if (strcmp(name, "channel_exit_water__volume_flow_rate") == 0) {
        *dest = hydro_get_water_discharge_ptr(self->data);
    } else if (strcmp(name, "channel_exit_water_x-section__width") == 0) {
        *dest = hydro_get_width_ptr(self->data);
    } else if (strcmp(name, "channel_exit_water_x-section__depth") == 0) {
        *dest = hydro_get_depth_ptr(self->data);
    } else if (strcmp(name, "channel_entrance_water__volume_flow_rate") == 0) {
        *dest = hydro_get_water_discharge_ptr(self->data);
    } else if (strcmp(name, "atmosphere_water__domain_mean_of_precipitation_leq-volume_flux") == 0) {
        *dest = hydro_get_precipitation_ptr(self->data);
    } else if (strcmp(name, "channel_exit_water_sediment~bedload__mass_flow_rate") == 0) {
        *dest = hydro_get_bedload_flux_ptr(self->data);
    } else {
        *dest = NULL; return BMI_FAILURE;
    }

    if (*dest)
        return BMI_SUCCESS;
    else
        return BMI_FAILURE;
}


int
get_value(BMI_Model * self, const char * name, void *dest)
{
    void *src = NULL;
    int nbytes = 0;

    if (self->get_value_ptr (self, name, &src) == BMI_FAILURE)
        return BMI_FAILURE;

    if (self->get_var_nbytes (self, name, &nbytes) == BMI_FAILURE)
        return BMI_FAILURE;

    memcpy(dest, src, nbytes);

    return BMI_SUCCESS;
}


static int
get_value_at_indices (BMI_Model *self, const char *name, void *dest,
    int * inds, int len)
{
    void *src = NULL;
    int itemsize = 0;

    if (self->get_value_ptr(self, name, &src) == BMI_FAILURE)
        return BMI_FAILURE;

    if (self->get_var_itemsize(self, name, &itemsize) == BMI_FAILURE)
        return BMI_FAILURE;

    { /* Copy the data */
        int i;
        int offset;
        void * ptr;
        for (i=0, ptr=dest; i<len; i++, ptr+=itemsize) {
            offset = inds[i] * itemsize;
            memcpy (ptr, src + offset, itemsize);
        }
    }

    return BMI_SUCCESS;
}


static int
set_value (BMI_Model *self, const char *name, void *array)
{
    void * dest = NULL;
    int nbytes = 0;

    if (self->get_value_ptr(self, name, &dest) == BMI_FAILURE)
        return BMI_FAILURE;

    if (self->get_var_nbytes(self, name, &nbytes) == BMI_FAILURE)
        return BMI_FAILURE;

    memcpy (dest, array, nbytes);

    return BMI_SUCCESS;
}


static int
set_value_at_indices (BMI_Model *self, const char *name, int * inds, int len,
    void *src)
{
    void * to = NULL;
    int itemsize = 0;

    if (self->get_value_ptr (self, name, &to) == BMI_FAILURE)
        return BMI_FAILURE;

    if (self->get_var_itemsize(self, name, &itemsize) == BMI_FAILURE)
        return BMI_FAILURE;

    { /* Copy the data */
        int i;
        int offset;
        void * ptr;
        for (i=0, ptr=src; i<len; i++, ptr+=itemsize) {
            offset = inds[i] * itemsize;
            memcpy (to + offset, ptr, itemsize);
        }
    }
    return BMI_SUCCESS;
}


BMI_Model*
register_bmi_hydrotrend(BMI_Model *model)
{
    model->data = (void*)new_data();

    model->initialize = initialize;
    model->update = update;
    model->update_until = update_until;
    // model->update_frac = update_frac;
    model->finalize = finalize;
    // model->run_model = NULL;

    model->get_component_name = get_component_name;
    model->get_input_item_count = get_input_var_name_count;
    model->get_output_item_count = get_output_var_name_count;
    model->get_input_var_names = get_input_var_names;
    model->get_output_var_names = get_output_var_names;

    model->get_var_grid = get_var_grid;
    model->get_var_type = get_var_type;
    model->get_var_units = get_var_units;
    model->get_var_itemsize = get_var_itemsize;
    model->get_var_nbytes = get_var_nbytes;
    model->get_var_location = get_var_location;
    model->get_current_time = get_current_time;
    model->get_start_time = get_start_time;
    model->get_end_time = get_end_time;
    model->get_time_units = get_time_units;
    model->get_time_step = get_time_step;

    model->get_value = get_value;
    model->get_value_ptr = get_value_ptr;
    model->get_value_at_indices = get_value_at_indices;

    model->set_value = set_value;
    // model->set_value_ptr = NULL;
    model->set_value_at_indices = set_value_at_indices;

    model->get_grid_rank = NULL;
    model->get_grid_size = NULL;
    model->get_grid_type = NULL;

    model->get_grid_node_count = NULL;

    return model;
}
