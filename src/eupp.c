#include <R.h>
#include <Rdefines.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "eccodes.h"


///* (1) Helper functions. */
//SEXP getListElement(SEXP list, const char *str)
//{
//  SEXP elmt, names;
//  PROTECT(elmt  = R_NilValue);
//  PROTECT(names = getAttrib(list, R_NamesSymbol));
//
//  for(int i = 0; i < length(list); i++) {
//    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
//      elmt = VECTOR_ELT(list, i);
//      break;
//    }
//  }
//  UNPROTECT(2);
//  return elmt;
//}
//
///* (1) Helper functions. */
//int getListInt(SEXP list, const char *str)
//{
//  SEXP res = getListElement(list, str);
//  return INTEGER(res)[0];
//}
//
///* (1) Helper functions. */
//double getListReal(SEXP list, const char *str)
//{
//  SEXP res = getListElement(list, str);
//  return REAL(res)[0];
//}
//
//
///* Combine two strings s1 and s2 with separator 'sep'.
// * 'sep' can also be an empty string ("").
// * @param sep character string, separator.
// * @param s1,s2 character strings.
// * @returns String, combined "<s1><sep><s2>". */
//char *join(const char* sep, const char* s1, const char* s2)
//{
//    char* result = malloc(strlen(s1) + strlen(s2) + strlen(sep) + 1);
//
//    if (result) // thanks @pmg
//    {
//        if ( strlen(s1) == 0 ) {
//            strcpy(result, s2);
//        } else {
//            strcpy(result, s1);
//            if ( strlen(sep) > 0 ) { strcat(result, sep); }
//            strcat(result, s2);
//        }
//    }
//
//    return result;
//}
//
///* If the user request includes a set of steps we have to check
// * whether or not these steps are available in the grib file.
// * This function returns 0 if the step is available in the gri
// * file or 9 if not.
// * @param stp the step to be checked (needle)
// * @param steps array of longs, available steps (haystack)
// * @param n length of array steps
// * @returns Returns 0 if "stp" is in "steps", 9 else.
// */
//int _getgrib_check_step_in_array_(long stp, long *steps, size_t n) {
//    int i;
//    for ( i = 0; i < n; i++ ) {
//        if ( stp == steps[i] ) { return 0; }
//    }
//    return 9;
//}
//
///* Checking grib files and extracting available steps/required
// * steps.
// * @param file character string, name of the file to be read.
// * @param unique_keys character array with grib key names to be checked.
// *      If the keys specified here are not unique (e.g., different fields
// *      or variable names or resolution) the script will exit.
// * @param steps_req double, can be of length 1 "-999." or a set of 
// *      steps t obe read. If "-999." all available steps form the
// *      grib files will be returned.
// * @param check integer pointer, will be set to 0 if evertyhing is ok,
// *      something >0 if there problems occured and the check failed.
// * @param TODO ...
// */
//void _getgrib_check_grib_fields_(const char *file, SEXP unique_keys, int *check)
//{
//
//    ///* Variables for ecCodes */
//    ///codes_handle *msg = NULL;    /* Grib message handler */
//    codes_index* grbidx = NULL;  /* Grib index */
//    int err = 0;                 /* Error flag variable */
//    int ret = 0;                 /* Index return value */
//    int i;
//
//    /* Create comma separated list for the grib index.
//     * A combination of "step" and what the user defines to be
//     * constant*/
//    char *grbkeys = "";
//    for ( i = 0; i < length(unique_keys); i++ ) {
//        grbkeys = join(",", grbkeys, CHAR(STRING_ELT(unique_keys, i)));
//    }
//    ///Rprintf("\n\n%s\n\n", grbkeys);
//
//    /* Create new index */
//    grbidx = codes_index_new(0, grbkeys, &ret);
//
//    /* Indexing the current file */
//    ret  = codes_index_add_file(grbidx, file);
//    if ( ret ) { Rprintf("error: %s\n",codes_get_error_message(ret)); exit(ret); }
//
//    /* For now I am expecting to have only one set of parameters in the
//     * grib file and that all grib messages do have the very same extent.
//     * Checking this using indizes here.
//     */
//    size_t keylen;
//    const char *key;
//    for ( i = 0; i < length(unique_keys); i++ ) {
//        key = CHAR(STRING_ELT(unique_keys, i));
//        CODES_CHECK(codes_index_get_size(grbidx, key, &keylen), 0);
//        if ( keylen != 1 ) {
//            Rprintf("unique key \"%s\" (as specified) is not unique!\n", key);
//            /* Release the index */
//            codes_index_delete(grbidx);
//            *check = 9; return; /* Nothing is ok! */
//        }
//    }
//
//
//    // Setting "return value" (void)
//    *check = 0; /* Return 0 (everything looks ok) */
//}
//
//
///* Creates a small double-vector which is used to compare the
// * content of different grib messages.
// * @param msg codes_handle object, message handler.
// * @return Returns a vector of doubles with (currently) 6 elements
// *      containing Ni/Nj (dimension of the grid) and lonmin/lonmax/latmin/latmax
// *      which specify the corners of the grid to check whether or not the
// *      domain is the very same.
// */
//double* _getgrib_get_grib_info_(codes_handle *msg)
//{
//    double* info;
//    info = malloc(sizeof(double)*6);
//
//    codes_get_double(msg, "Ni", &info[0]);
//    codes_get_double(msg, "Nj", &info[1]);
//    codes_get_double(msg, "longitudeOfFirstGridPointInDegrees", &info[2]);
//    codes_get_double(msg, "longitudeOfLastGridPointInDegrees",  &info[3]);
//    codes_get_double(msg, "latitudeOfFirstGridPointInDegrees",  &info[4]);
//    codes_get_double(msg, "latitudeOfLastGridPointInDegrees",   &info[5]);
//
//    return info;
//}
//
///* Compares the extent of two grib messages.
// * @param msg codes_handle message handler of the message to be checked
// * @param file character, name of the file where msg comes from (only for
// *      output/debugging purposes).
// * @param *ref double pointer, a vector returned by _getgrib_get_grib_info_(...)
// *      containing the values to be compared against.
// * @return No return values (void function), exit's if the meta information
// *      of msg and the referenc differ.
// */
//void _getgrib_check_grib_info_(codes_handle *msg, const char *file, double *ref)
//{
//    double *info;
//    info = _getgrib_get_grib_info_(msg);
//
//    for (int i = 0; i < 6; i++) {
//        if ( info[i] != ref[i] ) {
//            Rprintf("Grib info check error: %.5f != %.5f\nIn file \"%s\"\n",
//                    info[i], ref[i], file);
//            exit(9);
//        }
//    }
//
//    info = NULL;
//}

/* Reading grib data
 * @param file character string, name of the file to be read.
 * @param unique_keys character array with grib key names to be checked.
 *      If the keys specified here are not unique (e.g., different fields
 *      or variable names or resolution) the script will exit.
 */
SEXP eupp_grib_inventory(SEXP infile, SEXP unique_keys, SEXP check) {

    const char *file          = NULL;
    int        nprotected     = 0;  /* Number of protected elements */

    int nmsg           = 0;         /* Count number of messages to read */
    int err            = 0;         /* Error flag variable */
    FILE* fid          = NULL;      /* File connection handler */

    int i;                          /* Loop indices */

    /* eccodes objects/variables */
    file = CHAR(STRING_ELT(infile, 0)); /* Input file name */
    codes_handle   *msg   = NULL;       /* Grib message handler */

    // Getting input gribfile name as character
    Rprintf("C: %s\n", file);


    fid = fopen(file, "r");
    while ((msg = codes_handle_new_from_file(0, fid, PRODUCT_GRIB, &err)) != NULL) { nmsg += 1; }
    codes_handle_delete(msg);
    Rprintf("C: Number of messages to read: %d\n", nmsg);
    rewind(fid); /* go back to start without earning 2000 */

    /* Initialize return value for R */
    long Ni, Nj, Nens, level;
    double lat1, lat2, lon1, lon2;

    SEXP res_Nens = PROTECT(Rf_allocVector(REALSXP, nmsg));     ++nprotected;
    double *res_Nensptr = NULL; res_Nensptr = REAL(res_Nens);
    SEXP res_Ni = PROTECT(Rf_allocVector(REALSXP, nmsg));     ++nprotected;
    double *res_Niptr = NULL; res_Niptr = REAL(res_Ni);
    SEXP res_Nj = PROTECT(Rf_allocVector(REALSXP, nmsg));     ++nprotected;
    double *res_Njptr = NULL; res_Njptr = REAL(res_Nj);
    SEXP res_level = PROTECT(Rf_allocVector(REALSXP, nmsg));     ++nprotected;
    double *res_levelptr = NULL; res_levelptr = REAL(res_level);

    SEXP res_lon1 = PROTECT(Rf_allocVector(REALSXP, nmsg));     ++nprotected;
    double *res_lon1ptr = NULL; res_lon1ptr = REAL(res_lon1);
    SEXP res_lon2 = PROTECT(Rf_allocVector(REALSXP, nmsg));     ++nprotected;
    double *res_lon2ptr = NULL; res_lon2ptr = REAL(res_lon2);
    SEXP res_lat1 = PROTECT(Rf_allocVector(REALSXP, nmsg));     ++nprotected;
    double *res_lat1ptr = NULL; res_lat1ptr = REAL(res_lat1);
    SEXP res_lat2 = PROTECT(Rf_allocVector(REALSXP, nmsg));     ++nprotected;
    double *res_lat2ptr = NULL; res_lat2ptr = REAL(res_lat2);

    /* Character */
    size_t charlen = 20;
    char shortName[20]; 
    char typeOfLevel[20]; 

    SEXP res_shortName   = PROTECT(Rf_allocVector(STRSXP, nmsg));     ++nprotected;
    SEXP res_typeOfLevel = PROTECT(Rf_allocVector(STRSXP, nmsg));     ++nprotected;

    i = 0;
    while ((msg = codes_handle_new_from_file(0, fid, PRODUCT_GRIB, &err)) != NULL) {

        /* Getting message information */
        codes_get_long(msg, "numberOfForecastsInEnsemble", &Nens);
        codes_get_long(msg, "Ni", &Ni);
        codes_get_long(msg, "Nj", &Nj);
        codes_get_long(msg, "level", &level);

        codes_get_double(msg, "latitudeOfFirstGridPointInDegrees",  &lat1);
        codes_get_double(msg, "longitudeOfFirstGridPointInDegrees", &lon1);
        codes_get_double(msg, "latitudeOfLastGridPointInDegrees",   &lat2);
        codes_get_double(msg, "longitudeOfLastGridPointInDegrees",  &lon2);

        res_Nensptr[i]   = Nens;     res_levelptr[i]  = level;
        res_Niptr[i]     = Ni;       res_Njptr[i]     = Nj;
        res_lon1ptr[i]   = lon1;     res_lon2ptr[i]   = lon2;
        res_lat1ptr[i]   = lat1;     res_lat2ptr[i]   = lat2;

        codes_get_string(msg, "shortName", shortName, &charlen);
        SET_STRING_ELT(res_shortName, i, Rf_mkChar(shortName));
        codes_get_string(msg, "typeOfLevel", shortName, &charlen);

        SET_STRING_ELT(res_shortName, i, Rf_mkChar(shortName));

        codes_handle_delete(msg);
        ++i;
    }
    fclose(fid);

    Rprintf("asdfasdfasdfasdf\n\n");

    /* Constructing list return for R */
    SEXP res      = PROTECT(allocVector(VECSXP, 10));        ++nprotected;
    SEXP names    = PROTECT(Rf_allocVector(STRSXP, 10));     ++nprotected;
    SET_STRING_ELT(names, 0, Rf_mkChar("members"));
    SET_STRING_ELT(names, 1, Rf_mkChar("Ni"));
    SET_STRING_ELT(names, 2, Rf_mkChar("Nj"));
    SET_STRING_ELT(names, 3, Rf_mkChar("lon1"));
    SET_STRING_ELT(names, 4, Rf_mkChar("lon2"));
    SET_STRING_ELT(names, 5, Rf_mkChar("lat1"));
    SET_STRING_ELT(names, 6, Rf_mkChar("lat2"));
    SET_STRING_ELT(names, 7, Rf_mkChar("shortName"));
    SET_STRING_ELT(names, 8, Rf_mkChar("typeOfLevel"));
    SET_STRING_ELT(names, 9, Rf_mkChar("level"));
    Rf_setAttrib(res, R_NamesSymbol, names);

    SET_VECTOR_ELT(res, 0, res_Nens);
    SET_VECTOR_ELT(res, 1, res_Ni);
    SET_VECTOR_ELT(res, 2, res_Nj);
    SET_VECTOR_ELT(res, 3, res_lon1);
    SET_VECTOR_ELT(res, 4, res_lon2);
    SET_VECTOR_ELT(res, 5, res_lat1);
    SET_VECTOR_ELT(res, 6, res_lat2);
    SET_VECTOR_ELT(res, 7, res_shortName);
    SET_VECTOR_ELT(res, 8, res_typeOfLevel);
    SET_VECTOR_ELT(res, 9, res_level);

    /* Release protected elements and return to R */
    UNPROTECT(nprotected);
    return res;


}
