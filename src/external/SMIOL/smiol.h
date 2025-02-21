/*******************************************************************************
 * SMIOL -- The Simple MPAS I/O Library
 *******************************************************************************/
#ifndef SMIOL_H
#define SMIOL_H

#include "smiol_types.h"


/*
 * Library methods
 */
int SMIOL_fortran_init(MPI_Fint comm, int num_io_tasks, int io_stride,
                       struct SMIOL_context **context);
int SMIOL_init(MPI_Comm comm, int num_io_tasks, int io_stride,
               struct SMIOL_context **context);
int SMIOL_finalize(struct SMIOL_context **context);
int SMIOL_inquire(void);

/*
 * File methods
 */
int SMIOL_open_file(struct SMIOL_context *context, const char *filename,
                    int mode, struct SMIOL_file **file, size_t bufsize, int fformat);
int SMIOL_close_file(struct SMIOL_file **file);

/*
 * Dimension methods
 */
int SMIOL_define_dim(struct SMIOL_file *file, const char *dimname, SMIOL_Offset dimsize);
int SMIOL_inquire_dim(struct SMIOL_file *file, const char *dimname,
                      SMIOL_Offset *dimsize, int *is_unlimited);

/*
 * Variable methods
 */
int SMIOL_define_var(struct SMIOL_file *file, const char *varname, int vartype, int ndims, const char **dimnames);
int SMIOL_inquire_var(struct SMIOL_file *file, const char *varname, int *vartype, int *ndims, char **dimnames);
int SMIOL_put_var(struct SMIOL_file *file, const char *varname,
                  const struct SMIOL_decomp *decomp, const void *buf);
int SMIOL_get_var(struct SMIOL_file *file, const char *varname,
                  const struct SMIOL_decomp *decomp, void *buf);

/*
 * Attribute methods
 */
int SMIOL_define_att(struct SMIOL_file *file, const char *varname,
                     const char *att_name, int att_type, const void *att);

int SMIOL_inquire_att(struct SMIOL_file *file, const char *varname,
                      const char *att_name, int *att_type,
                      SMIOL_Offset *att_len, void *att);

/*
 * Control methods
 */
int SMIOL_sync_file(struct SMIOL_file *file);
const char *SMIOL_error_string(int errno);
const char *SMIOL_lib_error_string(struct SMIOL_context *context);
int SMIOL_set_option(void);
int SMIOL_set_frame(struct SMIOL_file *file, SMIOL_Offset frame);
int SMIOL_get_frame(struct SMIOL_file *file, SMIOL_Offset *frame);

/*
 * Decomposition methods
 */
int SMIOL_create_decomp(struct SMIOL_context *context,
                        size_t n_compute_elements, SMIOL_Offset *compute_elements,
                        int aggregation_factor,
                        struct SMIOL_decomp **decomp);
int SMIOL_free_decomp(struct SMIOL_decomp **decomp);

#endif
