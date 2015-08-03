#include "cdn-math-linear-algebra.h"

#include <Eigen/Eigen>

#include <vector>


// Hi-jacking memory model of Eigen is not easy. so we create our own
// stacks of solver to respect codyn way of a fixed stack size
// (avoiding dynamic memory allocation).
//
// (Taking the memory footprint of the solver is no sufficient. Indeed
// since we use dynamic size matrix, the internal of Eigen will
// allocate lot of memory everywhere, on a 16-bytes aligned boundaries
// (and we want to keep that for SIMD instructions). So for sure we
// loose the full stack size info, but we do not perform memory
// allocation outside of the *_workspace functions.
//
template <typename T,size_t solverDim = 2 ,size_t storageDim = 2>
class SolverStack 
{
	//empty definition, we use specialization for only a few cases
};

template <typename T>
class SolverStack<T,2,2> 
{
private :
	// It is not nice to use a vector of vector, but this is an easy
	// way to ensure constant access time (map would use far less
	// memory but would be O(nlog(n)) access time)
	static std::vector< std::vector<T*> > s_stack;
public :
	static T * Get(size_t rows, size_t cols) 
	{
		if (rows >= s_stack.size () || cols >= s_stack[rows - 1].size ()) 
		{
			return NULL;
		}
		return s_stack[rows - 1][cols - 1];
	}
	static void Allocate(size_t rows, size_t cols) 
	{
		if (rows >= s_stack.size ()) 
		{
			s_stack.resize (rows);
		}

		if (cols >= s_stack[rows - 1].size ()) 
		{
			s_stack[rows - 1].resize (cols, NULL);
		}

		if ( s_stack[rows -1][cols -1] != NULL ) 
		{
			return;
		}

		s_stack[rows - 1][cols - 1] = new T (rows, cols);
	}
};
template <typename T>

std::vector< std::vector<T*> > SolverStack<T,2,2>::s_stack;

// Case of a solver that can take non square matrix, but we know we
// will only use it for square matrices.
template <typename T>
class SolverStack<T,2,1>
{
private :
	static std::vector< T* > s_stack;
public :
	static T * Get(size_t rows) 
	{
		if (rows >= s_stack.size ()) 
		{
			return NULL;
		}
		return s_stack[rows - 1];
	}
static void Allocate(size_t rows) 
	{
		if (rows >= s_stack.size()) 
		{
			s_stack.resize (rows);
		}


		if (s_stack[rows -1] != NULL) 
		{
			return;
		}

		s_stack[rows - 1] = new T (rows, rows);
	}
};

template <typename T>
std::vector< T* > SolverStack<T,2,1>::s_stack;

// Case of a solver that only works on square matrices (like
// PartialPivLU, that only works on invertible matrices => square.
template <typename T>
class SolverStack<T,1,1> 
{
private :
	static std::vector< T* > s_stack;
public :
	static T * Get(size_t rows) 
	{
		if (rows >= s_stack.size ()) 
		{
			return NULL;
		}
		return s_stack[rows - 1];
	}
static void Allocate(size_t rows) 
	{
		if (rows >= s_stack.size ()) 
		{
			s_stack.resize (rows);
		}


		if ( s_stack[rows -1] != NULL ) 
		{
			return;
		}

		s_stack[rows - 1] = new T (rows);
	}
};

template <typename T>
std::vector< T* > SolverStack<T,1,1>::s_stack;


//we typedef the stack we use in our function
typedef SolverStack<Eigen::ColPivHouseholderQR<Eigen::MatrixXd>,2,1> StackColPivHouseholderQR;
typedef SolverStack<Eigen::JacobiSVD<Eigen::MatrixXd>,2,2 >          StackJacobiSVD;
typedef SolverStack<Eigen::HouseholderQR<Eigen::MatrixXd>,2,2 >      StackHouseholderQR;

extern "C" {
	
void
matrix_multiply (CdnStack           *stack,
                 CdnStackArgs const *argdim)
{
	gdouble *ptrA;
	gdouble *ptrB;
	gdouble *ptrC;

	gint num1 = argdim->args[1].rows * argdim->args[1].columns;
	gint num2 = argdim->args[0].rows * argdim->args[0].columns;
	gint numend = argdim->args[1].rows * argdim->args[0].columns;

	ptrC = cdn_stack_output_ptr (stack);
	ptrB = ptrC - num2;
	ptrA = ptrB - num1;

	//map the matrices to eigen.
	using namespace Eigen;
	Map<MatrixXd> C (ptrC, argdim->args[1].rows, argdim->args[0].columns);
	Map<MatrixXd> B (ptrB, argdim->args[1].columns,argdim->args[0].columns);
	Map<MatrixXd> A (ptrA, argdim->args[1].rows, argdim->args[1].columns);

	//noalias is important to avoid memory alocation. we already have
	//allocated our temp member C. no aliasing can occur here.
	C.noalias () = A * B;

	memmove (ptrA, ptrC, sizeof (gdouble) * numend);

	cdn_stack_set_output_ptr (stack,
	                          ptrA + numend);

};

void
op_inverse (CdnStack           *stack,
            CdnStackArgs const *argdim,
            gpointer            userdata)
{
	LP_int n;
	LP_double *ptr;
	LP_int nn;

	n = argdim->args[0].rows;
	nn = n * n;

	ptr = cdn_stack_output_ptr (stack) - nn;

	using namespace Eigen;
	Map<MatrixXd> a (ptr, n, n);

	ColPivHouseholderQR<MatrixXd> * solver = StackColPivHouseholderQR::Get (n);
	if ( solver == NULL ) 
	{
		//we resort to dynamic allocation :-(
		solver = new ColPivHouseholderQR<MatrixXd> (n, n);
	}

	solver->compute (a);

	a = solver->inverse ();
}

LP_int
inverse_work_space (CdnDimension const *dim)
{
	// no need to allocate on Codyn stack, we have our own internal stack for eigen solver.
	using namespace Eigen;
	StackColPivHouseholderQR::Allocate (dim->rows);

	return 0;
}

void
op_pseudo_inverse (CdnStack           *stack,
                   CdnStackArgs const *argdim,
                   gpointer            userdata)
{
	LP_int m = argdim->args[0].rows;
	LP_int n = argdim->args[0].columns;
	LP_double threshold;
	LP_int minSize = std::min (m, n);
	
	// reserved space is at output
	LP_double * sValuesPtr = cdn_stack_output_ptr (stack);
	// after singular values, we have the n * m tmp
	LP_double * tmpValue   = sValuesPtr + minSize;
	// A is just before reserved space
	LP_double * ptrA       = sValuesPtr - n * m;
	using namespace Eigen;
	// maps the matrix to eigen primitive
	Map<MatrixXd> A (ptrA, m, n);

	// get the solver pre-allocated memory
	Eigen::JacobiSVD<MatrixXd> * solver = StackJacobiSVD::Get (m, n);
	if ( solver == NULL ) 
	{
		// last resort to dynamic allocation
		solver = new JacobiSVD<MatrixXd> (m, n);
	}
	
	//computes the SVD decomposition
	solver->compute (A, ComputeThinU | ComputeThinV);

	// computes reciprocal, in the pre-alocated space
	Map<JacobiSVD<MatrixXd>::SingularValuesType> singularReciprocal (sValuesPtr, minSize);
	// singular values are stored in decreasing order, so no need to call singularValues().max() !
	threshold = std::numeric_limits<double>::epsilon () * std::max (m, n) * solver->singularValues () (0);
	for(int i = 0; i < minSize; ++i) 
	{
		if ( solver->singularValues () (i) > threshold ) 
		{
			singularReciprocal (i) = 1 / solver->singularValues () (i);
		} 
		else 
		{
			singularReciprocal (i) = 0.0;
		}
	}
	//we use a explicit multiplication temporary to avoid temporary memory allocation.
	Map<MatrixXd> tmp (tmpValue, n, minSize);
	// noalias to tell eigen not to use a temporary variable implicitely.
	tmp.noalias() = solver->matrixV() * singularReciprocal.asDiagonal();
	//we remap the result to A, beware dimension are now n x m.
	Map<MatrixXd> result (ptrA, n, m);
	result.noalias () = tmp * solver->matrixU ().adjoint ();
}

LP_int
pseudo_inverse_work_space (CdnDimension const *dim)
{
	//allocate a solver for the problem dimension
	using namespace Eigen;
	StackJacobiSVD::Allocate (dim->rows, dim->columns);
	// we would need to create a m-sized vector for singular values,
	// and one m*n matrix for temp value;
	return std::min (dim->rows, dim->columns) * (1 + dim->columns);
}


void
op_linsolve (CdnStack           *stack,
             CdnStackArgs const *argdim,
             gpointer            userdata)
{
	LP_int n;
	LP_int nrhs;

	LP_int numa = cdn_stack_arg_size (argdim->args);
	LP_int numb = cdn_stack_arg_size (argdim->args + 1);

	gdouble * ptrA = cdn_stack_output_ptr (stack) - numa;
	gdouble * ptrB = ptrA - numb;

	n    = argdim->args[0].rows;
	nrhs = argdim->args[1].columns;
   
	using namespace Eigen;
	Map<MatrixXd> A (ptrA, n, n);
	Map<MatrixXd> b (ptrB, n, nrhs);
	
	ColPivHouseholderQR<MatrixXd>  * solver = StackColPivHouseholderQR::Get (n);
	if ( solver == NULL ) 
	{
		//resolve to dynamic allocation =)
		solver = new ColPivHouseholderQR<MatrixXd> (n, n);
	}
	
	solver->compute (A);

	b = solver->solve (b);

	cdn_stack_popn (stack, numa);
}

LP_int
linsolve_work_space (CdnDimension const *dim)
{

	using namespace Eigen;
	StackColPivHouseholderQR::Allocate (dim->rows);

	return 0;
}


void
op_qr (CdnStack           *stack,
       CdnStackArgs const *argdim,
       gpointer            userdata)
{
	LP_int m = argdim->args[0].rows;
	LP_int n = argdim->args[0].columns;
	LP_int extra_results_size = m * m;

	gdouble * ptrA = cdn_stack_output_ptr (stack) - m * n;
	gdouble * ptrR = ptrA + extra_results_size;
	using namespace Eigen;

	Map<MatrixXd> A (ptrA, m, n);
	HouseholderQR<MatrixXd> * solver = StackHouseholderQR::Get (n, n);
	if ( solver == NULL )
	{
		//resolves to dynamic allocation :-(
		solver = new HouseholderQR<MatrixXd> (m, n);
	}

	solver->compute (A);

	Map<MatrixXd> resultQ (ptrA, m, m);
	Map<MatrixXd> resultR (ptrR, m, n);
	resultQ = solver->householderQ ();
	resultR = solver->matrixQR ().triangularView<Upper> ();

	// says that the stack has increased in size.
	cdn_stack_set_output_ptr (stack,
	                          ptrA + m * m  + m * n );
}

LP_int 
qr_work_space (CdnDimension const *dim)
{
	using namespace Eigen;
	StackHouseholderQR::Allocate (dim->rows, dim->columns);

	return 0;
}
	
}

