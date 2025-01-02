//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <iostream>
#include <string>
//=============================================================================
#include "nlsTypes_exports.h"
#include "Dimensions.hpp"
#include "Interface.hpp"
#include "Types.hpp"
#include "ArrayOfVector.hpp"
//=============================================================================
namespace Nelson {

NLSTYPES_IMPEXP ArrayOfVector
scalarArrayOfToArrayOfVector(const ArrayOf&);

class Data;
class HandleGenericObject;

/** Ordered data array, the base Nelson data type.
 * The ArrayOf class is the base class of all data types.  It represents
 * an ordered collection of data, indexed using an arbitrary number of
 * dimensions.  The ArrayOf class uses a seperate data class to store the
 * data.  It can contain an N-dimensional array of any of the following
 * data types:
 *   - NLS_GO_HANDLE
 *   - NLS_HANDLE
 *   - NLS_CELL_ARRAY - a heterogenous array - essentially an array of ArrayOfs
 *   - NLS_STRUCT_ARRAY - a structure array
 *   - NLS_UINT8 - unsigned, 8-bit integers
 *   - NLS_INT8  - signed, 8-bit integers
 *   - NLS_UINT16 - unsigned, 16-bit integers
 *   - NLS_INT16 - signed, 16-bit integers
 *   - NLS_UINT32 - unsigned, 32-bit integers
 *   - NLS_INT32 - signed, 32-bit integers
 *   - NLS_SINGLE - 32-bit floating point
 *   - NLS_DOUBLE - 64-bit floating point
 *   - NLS_SCOMPLEX - 32-bit complex floating point
 *   - NLS_DCOMPLEX - 64-bit complex floating point
 *   - NLS_CHAR - a string class
 *
 * The Dimensions class is used to record the dimensions of the given ArrayOf.
 * The Dimension class represents an n-tuple of integers that record the
 * number of elements along the array in each dimension.  The data is stored
 * in a generalization of "column-major" order.  Note that the array
 * class does \em not actually include the data - the data is stored in the
 * Data class, to which ArrayOf contains a pointer.  This design allows for
 * rapid copying of ArrayOf objects.
 */
class NLSTYPES_IMPEXP ArrayOf
{
private:
    std::string _name;
    /**
     * This is a pointer to our data object - which is shared between
     * different ArrayOf objects.  It is essentially the memory block
     * directly associated with this data object.
     */
    Data* dp;
    /** Get a binary map from an array over the given range.
     * This member function converts an array into a boolean vector,
     * mathematically $$b(a(i)-1) = \mathrm{true}, 0 \leq i < \mathrm{maxD}$$,
     * where $a(i)$ is the contents of the array (after calling
     * ArrayOf::toOrdinal).  This is essentially a shortcut to converting
     * the array to a logical array and then converting to a boolean
     * array.  Throws an exception if $$a(i)$$ is outside the range
     * $$1,\ldots,\mathrm{maxD}$.
     */
    bool* getBinaryMap(indexType);

    /**
     * Copy us from the source object.
     */
    void
    copyObject(const ArrayOf& copy);
    /**
     * Delete our contents.
     */
    void
    deleteContents();

    /** Get the internal index corresponding to a given field name.
     * Get the internal index corresponding to a given field name.  This
     * is the index into the fieldname array of the argument.  If the
     * argument is not found, a value of -1 is returned.
     */
    [[nodiscard]] int64
    getFieldIndex(const std::string& fieldName) const;
    [[nodiscard]] int64
    getFieldIndexFromList(const std::string& fName, const stringVector& fieldNames) const;

    /* Check all fieldnames are valid */
    static bool
    haveValidFieldNames(const stringVector& fieldnames);

    /* Check all fieldnames are unique */
    static bool
    haveUniqueFieldNames(const stringVector& fieldnames);

public:
    [[nodiscard]] std::string
    name() const;

    void
    name(const std::string& name);

    [[nodiscard]] std::wstring
    wname() const;

    /**
     * Allocate an array.
     */
    static void*
    allocateArrayOf(NelsonType /*type*/, indexType length,
        const stringVector& names = stringVector(), bool initializeValues = false);
    /** Convert us to an index type
     * Convert the current object to an ordinal one.  This has different
     * meanings for different data types.
     *  - For string types, this conversion is not defined.
     *  - For logical types, this is accomplished using a linear search (done in
     *      two passes - one to identify the length of
     *      the final array, and another to identify the indices of non-zero
     * values.
     *  - For double types, this is done by typecasting (truncation).  A warning
     *      is emitted if the source value is fractional (non-integer) or invalid
     *      (zero or negative).
     *  - For complex types, this is done by typecasting.  The complex part
     *      is ignored, and a warning that this is the case is emitted also.
     * Throws an exception for string, cell, structure types, or if a zero or
     * negative index is encountered.
     */
    void
    toOrdinalType();
    /**
     * Ensure we have at most one owner for our data - allows us to modify the
     * data without affecting other arrays.
     */
    void
    ensureSingleOwner();
    /**
     * Default constructor.
     */
    ArrayOf();
    /**
     * Copy constructor.
     */
    ArrayOf(const ArrayOf& copy);
    /**
     * Create an empty ArrayOf of the specified type.
     */
    ArrayOf(NelsonType type);
    /**
     * Create an ArrayOf with the specified contents.
     */
    ArrayOf(NelsonType, const Dimensions&, void*, bool sparse = false,
        const stringVector& = stringVector());
    /**
     * Destructor - free the data object.
     */
    ~ArrayOf();
    /**
     * Assignment operator.
     */
    void
    operator=(const ArrayOf& copy);
    /**
     * Get the reference count to our data object - useful for
     * debug purposes.
     */
    [[nodiscard]] indexType
    getReferenceCount() const;
    /**
     * Get rows (equivalent to getDimensions().getRows()
     */
    [[nodiscard]] indexType
    getRows() const;
    /**
     * Get columns (equivalent to getDimensions().getColumns()
     */
    [[nodiscard]] indexType
    getColumns() const;
    /**
     * returns the number of dimensions in the array A.
     */
    [[nodiscard]] indexType
    nDims() const;
    /**
     * Get the length of the array as a vector.  This is equivalent
     * to computing length(this(:)).
     */
    [[nodiscard]] indexType
    getElementCount() const;
    /**
     * Get a copy of our dimensions vector.
     */
    [[nodiscard]] Dimensions
    getDimensions() const;
    /**
     * Get the fieldnames.
     */
    [[nodiscard]] stringVector
    getFieldNames() const;
    /**
     * Get our length along the given dimension.
     */
    [[nodiscard]] indexType
    getDimensionLength(int) const;
    /** Get the contents of our data block as a (read-only) void* pointer.
     * Get the contents of our data block as a void* pointer.  The
     * resulting pointer is read only, so that no modifications can
     * be made to the contents of our array.  To modify
     * the contents, you must make a copy and use setDataPointer to replace the
     * current data.  This "copy-on-write" technique avoids copies on
     * references to variables -- a good thing in this interpreted
     * environment where read-references dominate the accesses to variables.
     * Another option is to use getReadWriteDataPointer, which returns a
     * pointer that is free of object aliases.
     */
    [[nodiscard]] const void*
    getDataPointer() const;
    [[nodiscard]] const void*
    getSparseDataPointer() const;
    /** Get the contents of our data block as a read-write void* pointer.
     * Get the contents of our data block as a read-write void*
     * pointer.  It ensures that our data block is not aliased (meaning
     * that no other array objects share the data block), prior
     * to returning the pointer. To do this,
     * we have to go through the following steps:
     *   - Check the number of owners of our byte array.
     *   - If there is only one owner for the byte array, return
     *      a non-const pointer to the data
     *   - If there is more than one owner, copy our data.
     */
    void*
    getReadWriteDataPointer();
    /** Set the contents of our data block to the supplied pointer.
     * Set the contents of our data block to the supplied pointer.
     * Ownership of the data block is passed to the array, i.e., the
     * caller should not manipulate the block in any way after
     * calling this function. To avoid recopying, ownership of
     * the byte-array is passed
     * to the reference counting data object Data at this point.
     * That means that the caller is _not_ responsible for freeing
     * the memory block.
     */
    void
    setDataPointer(void*);
    /** Resize an array.
     * Resize the array to a new set of dimensions.  This resize operation
     * puts the contents of the array at the (0,...,0) corner of the new
     * array.
     */
    void
    resize(Dimensions& a);
    /** Resize an array based on a vector indexing expression.
     * This method resizes an object so that an index of the type this(n)
     * is valid.  In particular, if "this" is a scalar, then this(n) extends
     * the array in the column dimension.  If "this" is a column vector, then
     * this(n) extends the array in the row dimension.  If "this" is a row
     * vector, then this(n) extends the array in the column direction.
     * For an arbitrarily dimensioned array, this(n) makes the array into
     * a row vector of length n.
     */
    void vectorResize(indexType);
    /** Reshape an array.
     * Reshape the array along a new set of dimensions.  Valid provided that
     * setting the dimensions of the array to a does not change the number of
     * elements in the array.
     * Throws an exception if the new dimension has a different number of elements
     * than we currently have.
     */
    void
    reshape(Dimensions& a, bool checkValidDimension = true);

    /** change dimensions of array (in place)
     * class and struct not supported
     * Throws an exception if the new dimension has a different number of elements
     * than we currently have.
     */
    void
    changeInPlaceDimensions(const Dimensions& a);

    /**
     * Get our data class (of type Class).
     */
    [[nodiscard]] NelsonType
    getDataClass() const;
    /**
     * Calculate the size of each element in this array.
     */
    [[nodiscard]] indexType
    getElementSize() const;
    /**
     * Calculate the bytes required to hold this array (element size * length)
     */
    [[nodiscard]] indexType
    getByteSize() const;
    /**
     * Returns true if we are (meaningfully) positive.  For the unsigned integer
     * types, this is always true.  For complex types, this is false.  For the
     * signed integer types or the floating point types, the result is based on a
     * linear scan through the array.
     */
    [[nodiscard]] bool
    isPositive() const;
    [[nodiscard]] bool
    isSparse() const;

    void
    makeSparse();
    void
    makeDense();
    [[nodiscard]] indexType
    getNonzeros() const;
    /**
     * Returns true if we match the scalar value in x.  For strings, this is done
     * by doing a string compare.  For numerical values, we promote to a common
     * type and do a comparison.
     */
    [[nodiscard]] bool
    testCaseMatchScalar(ArrayOf x) const;
    /**
     * Returns true if we match the argument x, or if x is a cell-array,
     * returns true if we match any of the cells in x.  Uses
     * ArrayOf::testCaseMatchScalar to do the actual testing. Throws an exception
     * for non-scalars (apart from strings) or reference types. Also throws an
     * exception if the argument is not either a scalar or a cell array.
     */
    [[nodiscard]] bool
    testForCaseMatch(ArrayOf x) const;
    /**
     * Returns TRUE if we are empty (we have no elements).
     */
    [[nodiscard]] bool
    isEmpty(bool allDimensionsIsZero = false) const;
    /**
     * Returns TRUE if we have only a single element.
     */
    [[nodiscard]] bool
    isScalar() const;
    /**
     * Returns TRUE if we are 2-Dimensional.
     */
    [[nodiscard]] bool
    is2D() const;
    /**
     * Returns TRUE if we are 2-Dimensional and rows == cols.
     */
    [[nodiscard]] bool
    isSquare() const;
    /**
     * Returns TRUE if we are a vector.
     */
    [[nodiscard]] bool
    isVector() const;

    [[nodiscard]] bool
    isRowVector() const;

    [[nodiscard]] bool
    isColumnVector() const;
    /**
     * Returns TRUE if we are a reference type (cell array or
     * struct array).
     */
    [[nodiscard]] bool
    isReferenceType() const;
    /**
     * Returns TRUE if we are a complex data type.
     */
    [[nodiscard]] bool
    isComplex() const;
    /**
     * Returns TRUE if we are a real data type.
     */
    [[nodiscard]] bool
    isReal() const;
    /**
     * Returns TRUE if all values are real.
     */
    [[nodiscard]] bool
    allReal() const;
    /**
     * Returns TRUE if we are a sparse double or complex data type.
     */
    [[nodiscard]] bool
    isSparseDoubleType(bool realOnly = false) const;

    /**
     * Returns TRUE if it is a ndarraydouble type (not sparse, not scalar, 2D matrix)
     */
    [[nodiscard]] bool
    isNdArrayDoubleType(bool realOnly = false) const;

    /**
     * Returns TRUE if it is a double type (not ndarray, not sparse)
     */
    [[nodiscard]] bool
    isDoubleType(bool realOnly = false) const;

    /**
     * Returns TRUE if it is a single type (not ndarray, not sparse)
     */
    [[nodiscard]] bool
    isSingleType(bool realOnly = false) const;

    /**
     * Returns TRUE if it is a NLS_DOUBLE or NLS_DCOMPLEX
     */
    [[nodiscard]] bool
    isDoubleClass() const;

    /**
     * Returns TRUE if it is a NLS_SINGLE or NLS_SCOMPLEX
     */
    [[nodiscard]] bool
    isSingleClass() const;

    /**
     * Returns TRUE if it is a ndarraysingle type (not sparse, not scalar, 2D matrix)
     */
    [[nodiscard]] bool
    isNdArraySingleType(bool realOnly = false) const;

    /**
     * Returns TRUE if we are a string.
     */
    [[nodiscard]] bool
    isCharacterArray() const;
    [[nodiscard]] bool
    isRowVectorCharacterArray() const;

    [[nodiscard]] bool
    isNdArrayCharacterType() const;

    [[nodiscard]] bool
    isIntegerType() const;
    [[nodiscard]] bool
    isIntegerValue() const;
    [[nodiscard]] bool
    isNdArrayIntegerType() const;

    /*
     * helpers function
     * NLS_UINT8, ..., NLS_UINT64
     */
    [[nodiscard]] bool
    isUnsignedIntegerType() const;

    /*
     * helpers function
     * NLS_INT8, ..., NLS_INT64
     */
    [[nodiscard]] bool
    isSignedIntegerType() const;

    /**
     * Copy data from our data array to the specified array.  This is a
     * deep copy, in the sense that pointers are copied by creating
     * new objects.  Copy count elements, starting at index srcIndex
     * to the destination address starting at index dstIndex.   The addresses
     * are in terms of indices, not bytes.
     */
    void
    copyElements(indexType srcIndex, void* dstPtr, indexType dstIndex, indexType count);
    /**
     * Promote our array to a new type.  For empty arrays, this type
     * promotion always succeeds.  For cell arrays, this does nothing (except
     * throw an error if we attempt to promote it to a different type).  For
     * structure arrays, promoting to a structure array has three possible
     * outcomes:
     *   - If the fields match in order and contents then the promotion is
     * successful.
     *   - If the fields match in contents but not in-order then the promotion
     * involves reordering the data.
     *   - If the fields match in contents but the destination type has
     *     additional fields, then the promotion involves reordering the data and
     *     adding space.
     * Throws an exception if
     *   - we try to convert a cell-array to another type.
     *   - we try to promote a structure-array to another array with
     *     an incompatible field setup (i.e., not one of the three outcomes
     *     listed above).
     *   - we try to convert a structure-array to a non-structure array type.
     *   - we try to convert any numerical types to a reference type.
     */
    void
    promoteType(NelsonType new_type, const stringVector& fieldNames);
    /**
     * Promote our array to a new type.  This is a shortcut for when new_type is
     * not NLS_STRUCT_ARRAY, so that the fieldNames argument is not needed.
     */
    void
    promoteType(NelsonType new_type);

    /**
     * returns if array can be promoted to new type
     */
    bool
    canBePromotedTo(NelsonType new_type);

    /**
     * Diagonal constructor - construct an array from a given vector, with
     * the contents of the vector stored into the specified diagonal of the
     * matrix.
     * Throwsn an exception if the argument is not a vector.
     */
    static ArrayOf
    diagonalConstructor(ArrayOf src, int64 diagonalOrder);

    /**
     * Get the diagonal elements of an array.  Only applicable to 2-dimensional arrays.
     * The diagonal part of a rectangular matrix
     * is a vector of length K.  For an M x N matrix, the L order diagonal has a length
     * that can be calculated as:
     *    K = min(M,N-L) for L > 0 or
     *    K = min(M+L,N) for L < 0
     * Throws an exception for multi-dimensional arrays.
     */
    ArrayOf
    getDiagonal(int64 diagonalOrder);

    /**
     * Empty constructor
     */
    static ArrayOf
    emptyCell(const Dimensions& dim);
    static ArrayOf
    emptyConstructor(const Dimensions& dim, bool bIsSparse = false);
    static ArrayOf
    emptyConstructor(indexType m = 0, indexType n = 0, bool bIsSparse = false);
    /**
     * Scalar constructor - Construct an NLS_LOGICAL object with a scalar
     * value.
     */
    static ArrayOf
    logicalConstructor(bool aval);
    /**
     * Scalar constructor - Construct an NLS_UINT8 object with a scalar
     * value.
     */
    static ArrayOf
    uint8Constructor(uint8 aval);
    /**
     * Scalar constructor - Construct an NLS_INT8 object with a scalar
     * value.
     */
    static ArrayOf
    int8Constructor(int8 aval);
    /**
     * Scalar constructor - Construct an NLS_UINT16 object with a scalar
     * value.
     */
    static ArrayOf
    uint16Constructor(uint16 aval);
    /**
     * Scalar constructor - Construct an NLS_INT16 object with a scalar
     * value.
     */
    static ArrayOf
    int16Constructor(int16 aval);
    /**
     * Scalar constructor - Construct an NLS_UINT32 object with a scalar
     * value.
     */
    static ArrayOf
    uint32Constructor(uint32 aval);
    /**
     * Scalar constructor - Construct an NLS_INT32 object with a scalar
     * value.
     */
    static ArrayOf
    int32Constructor(int32 aval);

    /**
     * Scalar constructor - Construct an NLS_UINT64 object with a scalar
     * value.
     */
    static ArrayOf
    uint64Constructor(uint64 aval);
    /**
     * Scalar constructor - Construct an NLS_INT64 object with a scalar
     * value.
     */
    static ArrayOf
    int64Constructor(int64 aval);

    /**
     * Scalar constructor - Construct an NLS_SINGLE object with a scalar
     * value.
     */
    static ArrayOf
    singleConstructor(float aval);
    /**
     * Scalar constructor - Construct an NLS_DOUBLE object with a scalar
     * value.
     */
    static ArrayOf
    doubleConstructor(double aval);
    /**
     * Complex constructor - Construct an NLS_SCOMPLEX object with a
     * complex scalar value.
     */
    static ArrayOf
    complexConstructor(float aval, float bval);
    /**
     * Complex constructor - Construct an NLS_DCOMPLEX object with a
     * complex scalar value.
     */
    static ArrayOf
    dcomplexConstructor(double aval, double bval);

    /**
     * Converts an string array to char array
     * raises an error if not string array
     * if missing value converted to '' or raises an error
     */
    static ArrayOf
    stringArrayToCharacterArray(const ArrayOf& stringArray, bool missingAsEmpty = true);

    /**
     * char array constructor - Construct an NLS_CHAR object with the given
     * vector strings as a value (char array).
     */
    static ArrayOf
    characterVectorToCharacterArray(const wstringVector& strs, bool leftAlign = true);

    static ArrayOf
    characterVectorToCharacterArray(const stringVector& strs, bool leftAlign = true);

    /**
     * String constructor - Construct an NLS_CHAR object with the given
     * string as a value.
     */
    static ArrayOf
    characterArrayConstructor(const std::string& astr);

    /**
     * String constructor - Construct an NLS_CHAR object with the given
     * string as a value.
     */
    static ArrayOf
    characterArrayConstructor(const std::wstring& astr);

    /**
     * uint8 vector constructor - Construct an NLS_UINT8 object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    uint8VectorConstructor(indexType len);

    /**
     * int8 vector constructor - Construct an NLS_INT8 object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    int8VectorConstructor(indexType len);

    /**
     * uint16 vector constructor - Construct an NLS_UINT16 object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    uint16VectorConstructor(indexType len);

    /**
     * int16 vector constructor - Construct an NLS_INT16 object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    int16VectorConstructor(indexType len);

    /**
     * uint32 vector constructor - Construct an NLS_UINT32 object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    uint32VectorConstructor(indexType len);

    /**
     * int32 vector constructor - Construct an NLS_INT32 object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    int32VectorConstructor(indexType len);

    /**
     * uint64 vector constructor - Construct an NLS_UINT64 object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    uint64VectorConstructor(indexType len);

    /**
     * int64 vector constructor - Construct an NLS_INT64 object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    int64VectorConstructor(indexType len);

    /**
     * Double vector constructor - Construct an NLS_DOUBLE object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    doubleVectorConstructor(indexType len);

    /**
     * Double vector constructor - Construct an NLS_DOUBLE object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    doubleVectorConstructor(std::vector<double> values);

    /**
     * Single vector constructor - Construct an NLS_SINGLE object
     * that is a (row) vector with the given length.
     */
    static ArrayOf
    singleVectorConstructor(indexType len);

    /**
     * int32 matrix constructor - Construct an NLS_INT32 object
     * that is a (row, columns) matrix with the given length.
     */
    static ArrayOf
    int32Matrix2dConstructor(indexType m, indexType n);

    /**
     * Double matrix constructor - Construct an NLS_DOUBLE object
     * that is a (row, columns) matrix with the given length.
     */
    static ArrayOf
    doubleMatrix2dConstructor(indexType m, indexType n);

    /**
     * Single matrix constructor - Construct an NLS_SINGLE object
     * that is a (row, columns) matrix with the given length.
     */
    static ArrayOf
    singleMatrix2dConstructor(indexType m, indexType n);

    /**
     * Construct a NLS_INT32 or NLS_INT64 (on x64 platform) vector (either
     * vertical or horizontal) corresponding to minval:stepsize:maxval, with an
     * optional transpose.
     */
    static ArrayOf
    integerRangeConstructor(indexType minval, indexType stepsize, indexType maxval, bool vertical);

    /**
     * The constructor for a cell array is significantly simpler than
     * the matrix constructor.  The argument is a list of rowdefs.  Each
     * element in the rowdef is a ArrayOf pointer that we assign to our
     * data pointer.  The only thing we need to make sure of is that
     * each row has the same number of elements in it.
     * Throws an exception if the geometry of the argumens is incompatible.
     */
    static ArrayOf
    cellConstructor(ArrayOfMatrix& m);

    /**
     * Converts a variable to a cell with the content
     * if m is a cell returned value is m
     */
    static ArrayOf
    toCell(ArrayOf m);

    /**
     * returns value as an array =A(index)
     * simple extraction (fast used 'for' loop)
     */
    ArrayOf
    getValueAtIndex(uint64 index);

    void
    setValueAtIndex(uint64 index, const ArrayOf& scalarValue);

    void
    setValue(const ArrayOf& value);
    /**
     * Get a subset of an ArrayOf.  This is for vector-indexing, meaning that
     * the argument is assumed to refer to the elements in their order as a
     * vector. So, x(10) is equivalent to x(:)(10), even if, say, x is 3 x 4.
     * Throws an exception if
     *  - the variable is empty
     *  - the argument subset exceeds our valid domain
     */
    ArrayOf
    getVectorSubset(ArrayOf& index);
    /**
     * Get a subset of an ArrayOf.  This if for n-Dimensional-indexing, meaning
     * that x(10) is really x(10,1).
     * Throws an exception if the variable is empty.
     */
    ArrayOf
    getNDimSubset(ArrayOfVector& index);
    /**
     * Get a subset of an ArrayOf using contents-addressing.  This is for vector-
     * indexing, meaning that the argument is assumed to refer to the elements in
     * their order as a vector.   So, x{10} is equivalent to x(:){10}, even if,
     * say, x is 3 x 4.  This function is only used in assignment calls of the
     * form e.g., a{10} = 5.  The vector argument must therefor be a scalar.
     * Throws an exception if
     *  - we are not a cell array
     *  - the argument is empty
     *  - the argument defines more than a single value
     *  - the index exceeds the bounds of the array.
     */
    ArrayOf
    getVectorContents(ArrayOf& index);
    /**
     * Get a subset of an ArrayOf using contents-addressing.  This is for NDim-
     * indexing, meaning that the argument is assumed to refer to the elements in
     * their N-Dimensional: meaning that x{10} is really x{10,1}.  Like
     * getVectorContents, this function is meant for assignments only.
     * Throws an exception if
     *  - we are not a cell array
     *  - the indices do not define a single value
     */
    ArrayOf
    getNDimContents(ArrayOfVector& index);
    /**
     * Get a subset of a (cell) ArrayOf using contents-addressing.  This is used
     * when a cell array is used to supply a list of expressions. Throws an
     * exception if
     *   - we are not a cell-array
     *   - the indices exceed the array bounds
     */
    ArrayOfVector
    getVectorContentsAsList(ArrayOf& index);
    /**
     * Get a subset of an ArrayOf using contents-addressing.  This is used when a
     * cell array is used to supply a list of expressions. Throws an exception if
     * we are not a cell-array.
     */
    ArrayOfVector
    getNDimContentsAsList(ArrayOfVector& index);
    /**
     * Set a subset of an ArrayOf.  Uses vector-indexing, meaning that the
     * argument is assumed to refer to the elements in their order as a vector.
     * So, x(10) is equivalent to x(:)(10), even if, say, x is 3 x 4.
     * Throws an exception if there is a size mismatch between the index and the
     * data.
     */
    void
    setVectorSubset(ArrayOf& index, ArrayOf& data);
    /**
     * Set a subset of an ArrayOf.   This if for n-Dimensional-indexing, meaning
     * that x(10) is really x(10,1).
     * Throws an exception if there is a size mismatch between the index and the
     * data.
     */
    void
    setNDimSubset(ArrayOfVector& index, ArrayOf& data);
    /**
     * Set a subset of an ArrayOf using contents-indexing, meaning that the
     * argument is assumed to refer to the elements in their order as a vector.
     * So, x{10} is equivalent to x(:){10}, even if, say, x is 3 x 4.
     * Throws an exception if
     *   - the index has more than one element in it
     *   - the index is less than 1
     */
    void
    setVectorContents(ArrayOf& index, ArrayOf& data);
    /**
     * Set a subset of an ArrayOf.   This if for n-Dimensional-indexing, meaning
     * that x{10} is really x{10,1}.
     * Throws an exception if the index is not a scalar
     */
    void
    setNDimContents(ArrayOfVector& index, ArrayOf& data);
    /**
     * Replace the contents of a field with the supplied array.  Only valid for
     * scalar structures.
     * Throws an exception if we are not a structure array or we are a
     * multi-element structure-array.
     */
    void
    setField(const std::string& fieldName, ArrayOf& data);
    /**
     * Set a subset of an ArrayOf using contents-indexing, meaning that the
     * argument is assumed to refer to the elements in their order as a vector.
     * So, x{10} is equivalent to x(:){10}, even if, say, x is 3 x 4.
     * This is used when a cell-array is used as the return of a
     * multi-function call, i.e.: [x{3:5}] = foo.
     * Throws an exception if the number of elements in data do not match
     * the number of indices in index.
     */
    void
    setVectorContentsAsList(ArrayOf& index, ArrayOfVector& data);
    /**
     * Set a subset of an ArrayOf.   This if for n-Dimensional-indexing, meaning
     * that x{10} is really x{10,1}.  This is used when a cell-array is used
     * as the return of a multi-function call, i.e.: [x{1,3:5}] = foo.
     * Throws an exception if the number of elements in data do not match
     * the number of indices covered by index (which is the product of the
     * number of elements in each dimension of index).
     */
    void
    setNDimContentsAsList(ArrayOfVector& index, ArrayOfVector& data);
    /**
     * Replace the contents of a field with the supplied array.  This is used
     * when a structure array is used to hold the return of a multi-function
     * call, i.e.: [x.foo] = foo
     * Throws an exception if
     *   - we are not a structure array
     *   - the number of elements in data is not equal to the number of elements
     * in our array.
     */
    void
    setFieldAsList(const std::string& fieldName, ArrayOfVector& data);
    /**
     * Delete a subset of this array using the argument for vector indexing.
     * This is _much_ simpler than the planar case.  Here, we simply:
     *   -  Create a deletion map from the index variable.
     *   -  Adjust the size of the output, and reshape to
     *       a vector.
     *   -  Copy (and skip) as necessary.
     * The result is then resized using the same rules as in vectorResize.
     */
    void
    deleteVectorSubset(ArrayOf& arg);
    /**
     * Delete a subset of this array using the arguments for n-Dimensional
     * indexing.  This method is the "planar" delete, meaning that its
     * designed to delete all the entries in an N-ary array in one dimension.
     * It cannot be used to create "holes" in an array.
     * Throws an exception if the argument contains more than one non-colon index
     */
    void
    deleteNDimSubset(ArrayOfVector& args);
    /**
     * Get our contents as a C-string (UTF-8). Only works for STRING types.
     * Throws an exception for non-string types.
     */
    [[nodiscard]] std::string
    getContentAsCString() const;

    /**
     * Get our contents as a wide string (UTF-16). Only works for STRING types.
     * Throws an exception for non-string types.
     */
    [[nodiscard]] std::wstring
    getContentAsWideString(size_t lengthMax = std::string::npos) const;

    [[nodiscard]] std::wstring
    getContentAsArrayOfCharacters() const;

    /**
     * Get our contents as a C char * (pointer allocated with new). Only works for
     * STRING types. Throws an exception for non-string types.
     */
    [[nodiscard]] char*
    getContentAsCharactersPointer() const;

    /**
     * Get our contents as a C wchar_t * (pointer allocated with new). Only works
     * for STRING types. Throws an exception for non-string types.
     */
    [[nodiscard]] wchar_t*
    getContentAsWideCharactersPointer() const;

    /**
     * Get our contents as a vector wide string (UTF-16). Only works for CELL of
     * STRING types. no check on dimensions
     */
    [[nodiscard]] wstringVector
    getContentAsWideStringVector(bool bCheckVector = true) const;
    [[nodiscard]] stringVector
    getContentAsCStringVector(bool bCheckVector = true) const;

    /**
     * Get our contents as a vector wide string (UTF-16). Only works for CELL of
     * STRING types. Check if it is a cell with a row vector dimension
     */
    [[nodiscard]] wstringVector
    getContentAsWideStringRowVector() const;
    [[nodiscard]] stringVector
    getContentAsCStringRowVector() const;

    /**
     * Get our contents as a vector wide string (UTF-16). Only works for CELL of
     * STRING types. Check if it is a cell with a column vector dimension
     */
    [[nodiscard]] wstringVector
    getContentAsWideStringColumnVector() const;
    [[nodiscard]] stringVector
    getContentAsCStringColumnVector() const;

    /**
     * Get our contents as an logical scalar.
     * Throws an exception if we are not a scalar logical type.
     */
    [[nodiscard]] logical
    getContentAsLogicalScalar(bool arrayAsScalar = false) const;

    /**
     * Get our contents as an integer 8 bits scalar.
     * Throws an exception if we are not a scalar integer type.
     */
    [[nodiscard]] int8
    getContentAsInteger8Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as an unsigned integer 8 bits scalar.
     * Throws an exception if we are not a scalar integer type.
     */
    [[nodiscard]] uint8
    getContentAsUnsignedInteger8Scalar(
        bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as an integer scalar.
     * Throws an exception if we are not a scalar integer type.
     */
    [[nodiscard]] int16
    getContentAsInteger16Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as an unsigned integer scalar.
     * Throws an exception if we are not a scalar integer type.
     */
    [[nodiscard]] uint16
    getContentAsUnsignedInteger16Scalar(
        bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as an integer scalar.
     * Throws an exception if we are not a scalar integer type.
     */
    [[nodiscard]] int32
    getContentAsInteger32Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as an unsigned integer scalar.
     * Throws an exception if we are not a scalar integer type.
     */
    [[nodiscard]] uint32
    getContentAsUnsignedInteger32Scalar(
        bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as a double scalar.
     * Throws an exception if we are not scalar or cannot meaningfully
     * be converted to a double precision value.
     */
    [[nodiscard]] double
    getContentAsDoubleScalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as a double vector.
     * Throws an exception if cannot meaningfully
     * be converted to a double precision value.
     */

    [[nodiscard]] std::vector<double>
    getContentAsDoubleVector() const;

    /**
     * Get our contents as an unsigned integer scalar 64.
     * Throws an exception if we are not a scalar integer type.
     */
    [[nodiscard]] uint64
    getContentAsUnsignedInteger64Scalar(
        bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as an integer scalar 64.
     * Throws an exception if we are not a scalar integer type.
     */
    [[nodiscard]] int64
    getContentAsInteger64Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as a index type scalar.
     * Throws an exception if we are not scalar or cannot meaningfully
     * be converted to a index typevalue.
     */
    [[nodiscard]] indexType
    getContentAsScalarIndex(bool bWithZero = true, bool checkIsIntegerValue = false,
        bool convertNegativeValueAsZero = false) const;

    indexType*
    getContentAsIndexPointer();

    [[nodiscard]] std::vector<indexType>
    getContentAsIndexVector();

    /**
     * Get our contents as a double complex scalar.
     * Throws an exception if we are not scalar or cannot meaningfully
     * be converted to a double precision value.
     */
    [[nodiscard]] doublecomplex
    getContentAsDoubleComplexScalar(bool arrayAsScalar = false) const;

    /**
     * Get our contents as a float scalar.
     * Throws an exception if we are not scalar or cannot meaningfully
     * be converted to a double precision value.
     */
    [[nodiscard]] single
    getContentAsSingleScalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;

    /**
     * Get our contents as a single complex scalar.
     * Throws an exception if we are not scalar or cannot meaningfully
     * be converted to a single precision value.
     */
    [[nodiscard]] std::complex<single>
    getContentAsSingleComplexScalar(bool arrayAsScalar = false) const;

    /**
     * Returns the number of nonzero elements in the array.  For reference
     * types, this is a best-guess.
     */
    [[nodiscard]] indexType
    nnz() const;

    /*
     * Amount of storage allocated for nonzero matrix elements
     */
    [[nodiscard]] indexType
    nzmax() const;

    /*
     * number of elements
     */
    [[nodiscard]] indexType
    numel() const;

    [[nodiscard]] bool
    isCell() const;

    [[nodiscard]] bool
    isCellArrayOfCharacterVectors() const;

    [[nodiscard]] static ArrayOf
    toCellArrayOfCharacterRowVectors(const stringVector& elements);

    [[nodiscard]] static ArrayOf
    toCellArrayOfCharacterRowVectors(const wstringVector& elements);

    [[nodiscard]] static ArrayOf
    toCellArrayOfCharacterColumnVectors(const stringVector& elements);

    [[nodiscard]] static ArrayOf
    toCellArrayOfCharacterColumnVectors(const wstringVector& elements);

    [[nodiscard]] bool
    isLogical() const;
    [[nodiscard]] bool
    isNdArrayLogical() const;
    [[nodiscard]] bool
    isSparseLogicalType() const;

    [[nodiscard]] bool
    isNumeric() const;

    /** Compute the maximum index.
     * This computes the maximum value of the array as an index (meaning
     * that it must be greater than 0.  Because this is an internal function, it
     * assumes that the variable has already been passed through toOrdinalType()
     * successfully.  Throws an exception if the maximum value is zero or
     * negative.
     */
    indexType
    getMaxAsIndex();

    void
    deleteArrayOf(void* dp, NelsonType dataclass);

    //=========================================================================
    // common struct, class array
    //=========================================================================

    /**
     * Get the contents of a field from its field name.  Again, like
     * getVectorContents and getNDimContents, this function is meant for
     * assignments only, and the argument must be a scalar structure. Throws an
     * exection if we are a vector, or if the supplied field do not exist.
     */
    [[nodiscard]] ArrayOf
    getField(const std::string& fieldName) const;
    /**
     * Add another fieldname to our structure array.
     */
    indexType
    insertFieldName(const std::string& fieldName);
    /**
     * Get the contents of a field as an array from its field name.  This is used
     * when a structure array is used to supply a list of expressions.
     * Throws an exception if
     *   - we are not a structure array
     *   - the field does not exist
     */
    ArrayOfVector
    getFieldAsList(const std::string& fieldName) const;

    //=========================================================================
    // struct array
    //=========================================================================

    /**
     * Structure constructor - this is equivalent to the built in struct command.
     * First, we have to make sure that each entry of "values" have
     *  -  cell arrays of the same size (say MxN)
     *  -  single element cell arrays,
     *  -  single values.
     * With such a setup, the output is a structure array of size MxN.  Elements
     * which are defined by a single value or a single-element cell array are
     * replicated throughout all MxN entries.  Remaining elements take their
     * values from the cell-array.
     * Throws an exception if
     *  - the number of entries in the fieldnames vector does not match
     *    the number of entries in the values vector
     *  - the non-scalar values do not agree in dimension
     */
    static ArrayOf
    structConstructor(const stringVector& fNames, const ArrayOfVector& values);
    static ArrayOf
    structConstructor(const wstringVector& fNames, const ArrayOfVector& values);

    static ArrayOf
    emptyStructWithoutFields();
    static ArrayOf
    emptyStructConstructor(const stringVector& fNames, Dimensions& dim);
    static ArrayOf
    emptyStructConstructor(const wstringVector& fNames, Dimensions& dim);

    static ArrayOf
    structScalarConstructor(const stringVector& fNames, const ArrayOfVector& values);
    static ArrayOf
    structScalarConstructor(const wstringVector& fNames, const ArrayOfVector& values);

    [[nodiscard]] bool
    isStruct() const;

    //=========================================================================
    // function_handle array
    //=========================================================================

    static ArrayOf
    functionHandleConstructor(function_handle fptr);
    static ArrayOf
    functionHandleConstructor(const std::wstring& functionName, const std::wstring& anonymous);

    [[nodiscard]] function_handle
    getContentAsFunctionHandle() const;

    [[nodiscard]] bool
    isFunctionHandle() const;

    //=========================================================================
    // handle array
    //=========================================================================

    /*
     * check is handle type
     */
    [[nodiscard]] bool
    isHandle() const;
    /*
     * handle constructors
     */
    static ArrayOf
    handleConstructor(HandleGenericObject* ptr);
    static ArrayOf
    handleConstructor(nelson_handle hl);
    /*
     * check that scalar handle have an property name
     */
    [[nodiscard]] bool
    isHandleProperty(const std::wstring& propertyName) const;
    [[nodiscard]] bool
    isHandleProperty(const std::string& propertyName) const;

    /*
     * check that scalar handle have an method name
     */
    [[nodiscard]] bool
    isHandleMethod(const std::wstring& methodName) const;
    [[nodiscard]] bool
    isHandleMethod(const std::string& methodName) const;
    /*
     * get handle category
     * example py
     */
    [[nodiscard]] std::string
    getHandleCategory() const;
    /*
     * get handle class name
     * example py.int
     */
    [[nodiscard]] std::string
    getHandleClassName() const;
    /*
     * return handle as HandleGenericObject*
     */
    [[nodiscard]] HandleGenericObject*
    getContentAsHandleScalar() const;

    //=========================================================================
    // class array
    //=========================================================================
    [[nodiscard]] bool
    isClassType() const;

    void
    setClassType(const std::string& structname);
    void
    setClassType(const std::wstring& structname);

    [[nodiscard]] std::string
    getClassType() const;

    static ArrayOf
    classConstructor(const std::wstring& classTypeName, const wstringVector& fNames,
        const ArrayOfVector& values);
    static ArrayOf
    classConstructor(
        const std::string& classTypeName, const stringVector& fNames, const ArrayOfVector& values);

    static ArrayOf
    emptyClassConstructor(
        const std::wstring& className, const wstringVector& fNames, Dimensions& dim);
    static ArrayOf
    emptyClassConstructor(
        const std::string& className, const stringVector& fNames, Dimensions& dim);

    //=========================================================================
    // string array
    //=========================================================================
    /**
     * @brief Checks if the array is a string array.
     * @return true if the array is a string array, false otherwise.
     */
    [[nodiscard]] bool
    isStringArray() const;

    /**
     * @brief Checks if the array is an NdArray of strings.
     * @return true if the array is an NdArray of strings, false otherwise.
     */
    [[nodiscard]] bool
    isNdArrayString() const;

    /**
     * @brief Checks if the array is a scalar string array.
     * @param withMissing Flag indicating whether to consider missing values.
     * @return true if the array is a scalar string array, false otherwise.
     */
    [[nodiscard]] bool
    isScalarStringArray(bool withMissing = false) const;

    /**
     * @brief Constructs a string array from a std::string value.
     * @param value The std::string value to be converted to a string array.
     * @return An ArrayOf object representing the string array.
     */
    static ArrayOf
    stringArrayConstructor(const std::string& value);

    /**
     * @brief Constructs a string array from a std::wstring value.
     * @param value The std::wstring value to be converted to a string array.
     * @return An ArrayOf object representing the string array.
     */
    static ArrayOf
    stringArrayConstructor(const std::wstring& value);

    /**
     * @brief Constructs a string array with all elements initialized to Missing.
     * @param dims The dimensions of the string array.
     * @return An ArrayOf object representing the string array.
     */
    static ArrayOf
    stringArrayConstructorAllMissing(Dimensions& dims);

    /**
     * @brief Constructs a string array from a vector of strings and dimensions.
     * @param values The vector of strings to be converted to a string array.
     * @param dims The dimensions of the string array.
     * @return An ArrayOf object representing the string array.
     */
    static ArrayOf
    stringArrayConstructor(const stringVector& values, const Dimensions& dims);

    /**
     * @brief Constructs a string array from a vector of wstrings and dimensions.
     * @param values The vector of wstrings to be converted to a string array.
     * @param dims The dimensions of the string array.
     * @return An ArrayOf object representing the string array.
     */
    static ArrayOf
    stringArrayConstructor(const wstringVector& values, const Dimensions& dims);

    /**
     * Converts a variable to a string array with the content
     * if m is a string array returned value is m
     */
    static ArrayOf
    toStringArray(ArrayOf m, bool& needToOverload);
    //=========================================================================
    // graphics object handle
    //=========================================================================
    /*
     * check is graphics object handle type
     */
    [[nodiscard]] bool
    isGraphicsObject() const;

    static ArrayOf
    graphicsObjectConstructor(go_handle graphicsObjectHandle);

    /*
     * return nelson_handle as void *
     */
    [[nodiscard]] go_handle
    getContentAsGraphicsObjectScalar() const;

    //=========================================================================
    // Table class object
    //=========================================================================
    /*
     * check is Table type
     */
    [[nodiscard]] bool
    isTable() const;

    /**
     * @brief Constructs a table class object.
     *
     * This function creates and returns a table represented as an `ArrayOf` object,
     * based on the provided column values, variable (column) names, and row names.
     *
     * @param columnValues A collection of vectors, where each vector represents the
     *                     values in a single column of the table. All columns should
     *                     have the same number of rows for consistency.
     * @param variableNames(optional: empty)  A vector of strings representing the names of the
     * columns in the table. The size of this vector should match the number of columns in
     * `columnValues`.
     * @param rowNames (optional: empty) A vector of strings representing the names of the rows in
     * the table. The size of this vector should match the number of rows in each column of
     * `columnValues`.
     *
     * @return An `ArrayOf` object representing the constructed table, with the
     *         specified column values, variable names, and row names.
     */

    static ArrayOf
    tableConstructor(const ArrayOfVector& columnValues, const stringVector& variableNames,
        const stringVector& rowNames);
};
//=========================================================================
bool
isColonOperator(const ArrayOf& a);
//=========================================================================
constIndexPtr*
ProcessNDimIndexes(bool preserveColons, Dimensions& dims, ArrayOfVector& index, bool& anyEmpty,
    indexType& colonIndex, Dimensions& outDims, bool argCheck);
//=========================================================================
} // namespace Nelson
//=========================================================================
