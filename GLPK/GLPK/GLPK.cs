using System;
using System.Runtime.InteropServices;

namespace GLPK
{
    public unsafe static class GLPK
    {
        const string glpkLibrary = "glpk_4_53.dll";
        /* optimization direction flag: */
        static readonly int GLP_MIN = 1;  /* minimization */
        static readonly int GLP_MAX = 2;  /* maximization */

        /* kind of structural variable: */
        static readonly int GLP_CV = 1;  /* continuous variable */
        static readonly int GLP_IV = 2;  /* integer variable */
        static readonly int GLP_BV = 3;  /* binary variable */

        /* type of auxiliary/structural variable: */
        static readonly int GLP_FR = 1;  /* free (unbounded) variable */
        static readonly int GLP_LO = 2;  /* variable with lower bound */
        static readonly int GLP_UP = 3;  /* variable with upper bound */
        static readonly int GLP_DB = 4;  /* double-bounded variable */
        static readonly int GLP_FX = 5;  /* fixed variable */

        /* enable/disable flat */
        static readonly int GLP_ON = 1;
        static readonly int GLP_OFF = 0;

        [DllImport(glpkLibrary, SetLastError = true)]
        static extern double* glp_create_prob();
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_add_rows(double* lp, int rows);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_add_cols(double* lp, int cols);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_set_col_bnds(double* lp, int col, int bound_type, double lower_bound, double upper_bound);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_set_col_kind(double* lp, int col, int kind);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_load_matrix(double* lp, int elements, int* ia, int* ja, double* ar);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_set_obj_coef(double* lp, int j, double coef);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_set_row_bnds(double* lp, int row, int bound_type, double lower_bound, double upper_bound);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_simplex(double* lp, void* options);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_intopt(double* lp, void* options);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern double glp_mip_col_val(double* lp, int col);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern void glp_delete_prob(double* lp);
        [DllImport(glpkLibrary, SetLastError = true)]
        static extern int glp_term_out(int flag);
        static int[] reindexArray(int[] xs)
        {
            var ys = new int[xs.Length + 1];
            for (int i = 0; i < xs.Length; i++)
            {
                ys[i + 1] = xs[i] + 1;
            }
            return ys;
        }
        static double[] reindexArray(double[] xs)
        {
            var ys = new double[xs.Length + 1];
            Array.Copy(xs, 0, ys, 1, xs.Length);
            return ys;
        }
        // minimize fx subject to Ax <= rhs where all x are binary variables
        public static bool[] BinProgMin(double[] f, Tuple<int[],int[],double[]> A, double[] b,bool disableTermOutput = false)
        {
            if (disableTermOutput) 
                glp_term_out(GLP_OFF); 
            else 
                glp_term_out(GLP_ON);
            var mip = glp_create_prob();
            glp_add_cols(mip, f.Length);
            for (int i = 0; i < f.Length; i++)
            {
                glp_set_obj_coef(mip, i+1, f[i]);
                glp_set_col_kind(mip, i+1, GLP_BV);
            }
            glp_add_rows(mip, b.Length);
            for (int i = 0; i < b.Length; i++)
            {
                glp_set_row_bnds(mip, i+1, GLP_UP, 0.0, b[i]);
            }
            fixed (int* ia = reindexArray(A.Item1), ja = reindexArray(A.Item2))
            {
                fixed (double* ar = reindexArray(A.Item3))
                {
                    glp_load_matrix(mip, A.Item1.Length, ia, ja, ar);
                }
            }
            glp_simplex(mip, null);
            glp_intopt(mip, null);
            var result = new bool[f.Length];
            for (int i = 0; i < result.Length; i++)
            {
                result[i] = (int)glp_mip_col_val(mip, i + 1) == 1;
            }
            glp_delete_prob(mip);
            return result;
        }
    }
}
