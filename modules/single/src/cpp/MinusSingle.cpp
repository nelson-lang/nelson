//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "MinusSingle.hpp"
#include "MatrixCheck.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf single_matrix_matrix_subtraction(const ArrayOf &a,
                                                const ArrayOf &b) {
  Dimensions dimsC = a.getDimensions();
  indexType Clen = dimsC.getElementCount();
  void *Cp = new_with_exception<single>(Clen, false);
  Eigen::Map<Eigen::MatrixXf> matC((single *)Cp, 1, Clen);
  Eigen::Map<Eigen::MatrixXf> matA((single *)a.getDataPointer(), 1, Clen);
  Eigen::Map<Eigen::MatrixXf> matB((single *)b.getDataPointer(), 1, Clen);
  matC = matA - matB;
  return ArrayOf(NLS_SINGLE, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf singlecomplex_matrix_matrix_subtraction(const ArrayOf &a,
                                                       const ArrayOf &b) {
  Dimensions dimsC = a.getDimensions();
  indexType Clen = dimsC.getElementCount();
  void *Cp = new_with_exception<single>(Clen * 2, false);
  singlecomplex *Cz = reinterpret_cast<singlecomplex *>(Cp);
  Eigen::Map<Eigen::MatrixXcf> matC(Cz, 1, Clen);
  singlecomplex *Az =
      reinterpret_cast<singlecomplex *>((single *)a.getDataPointer());
  Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, Clen);
  singlecomplex *Bz =
      reinterpret_cast<singlecomplex *>((single *)b.getDataPointer());
  Eigen::Map<Eigen::MatrixXcf> matB(Bz, 1, Clen);
  matC = matA - matB;
  return ArrayOf(NLS_SCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf single_scalar_matrix_subtraction(ArrayOf &a, ArrayOf &b,
                                                bool reverse = false) {
  Dimensions dimsC = b.getDimensions();
  indexType Clen = dimsC.getElementCount();
  void *Cp = new_with_exception<single>(Clen, false);
  Eigen::Map<Eigen::MatrixXf> matC((single *)Cp, 1, Clen);
  Eigen::Map<Eigen::MatrixXf> matB((single *)b.getDataPointer(), 1, Clen);
  if (reverse) {
    matC = matB.array() - a.getContentAsSingleScalar();
  } else {
    matC = a.getContentAsSingleScalar() - matB.array();
  }
  return ArrayOf(NLS_SINGLE, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf singlecomplex_scalar_matrix_subtraction(ArrayOf &a, ArrayOf &b,
                                                       bool reverse = false) {
  Dimensions dimsC = b.getDimensions();
  indexType Clen = dimsC.getElementCount();
  void *Cp = new_with_exception<single>(Clen * 2, false);
  single *da = (single *)a.getDataPointer();
  singlecomplex *Az = reinterpret_cast<singlecomplex *>(da);
  singlecomplex *Cz = reinterpret_cast<singlecomplex *>(Cp);
  Eigen::Map<Eigen::MatrixXcf> matC(Cz, 1, Clen);
  singlecomplex *Bz =
      reinterpret_cast<singlecomplex *>((single *)b.getDataPointer());
  Eigen::Map<Eigen::MatrixXcf> matB(Bz, 1, Clen);
  if (reverse) {
    matC = matB.array() - Az[0];
  } else {
    matC = Az[0] - matB.array();
  }
  return ArrayOf(NLS_SCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
static void single_vector_subtraction(single *C, const single *A, indexType NA,
                                      const single *B, indexType NB,
                                      bool reverse = false) {
  indexType m = 0;
  if (reverse) {
    for (indexType i = 0; i < NA; i++) {
      for (indexType j = 0; j < NB; j++) {
        C[m] = B[j] - A[i];
        m++;
      }
    }
  } else {
    for (indexType i = 0; i < NA; i++) {
      for (indexType j = 0; j < NB; j++) {
        C[m] = A[i] - B[j];
        m++;
      }
    }
  }
}
//=============================================================================
static void singlecomplex_vector_subtraction(single *C, single *A, indexType NA,
                                             single *B, indexType NB,
                                             bool reverse = false) {
  indexType m = 0;
  singlecomplex *Az = reinterpret_cast<singlecomplex *>(A);
  singlecomplex *Bz = reinterpret_cast<singlecomplex *>(B);
  singlecomplex *Cz = reinterpret_cast<singlecomplex *>(C);
  if (reverse) {
    for (indexType i = 0; i < NA; i++) {
      for (indexType j = 0; j < NB; j++) {
        Cz[m] = Bz[j] - Az[i];
        m++;
      }
    }
  } else {
    for (indexType i = 0; i < NA; i++) {
      for (indexType j = 0; j < NB; j++) {
        Cz[m] = Az[i] - Bz[j];
        m++;
      }
    }
  }
}
//=============================================================================
static ArrayOf single_vector_matrix_subtraction(const ArrayOf &a,
                                                const ArrayOf &b,
                                                bool reverse = false) {
  const single *ptrA = (const single *)a.getDataPointer();
  const single *ptrB = (const single *)b.getDataPointer();
  indexType q = 0;
  Dimensions dimsC = b.getDimensions();
  indexType Clen = dimsC.getElementCount();
  void *Cp = new_with_exception<single>(Clen, false);
  single *C = (single *)Cp;
  if (reverse) {
    for (indexType i = 0; i < dimsC.getRows(); i++) {
      for (indexType j = 0; j < dimsC.getColumns(); j++) {
        indexType m = i + j * a.getDimensions().getRows();
        C[m] = ptrB[m] - ptrA[q];
      }
      q++;
    }
  } else {
    for (indexType i = 0; i < dimsC.getRows(); i++) {
      for (indexType j = 0; j < dimsC.getColumns(); j++) {
        indexType m = i + j * a.getDimensions().getRows();
        C[m] = ptrA[q] - ptrB[m];
      }
      q++;
    }
  }
  return ArrayOf(NLS_SINGLE, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf singlecomplex_vector_matrix_subtraction(const ArrayOf &a,
                                                       const ArrayOf &b,
                                                       bool reverse = false) {
  Dimensions dimsC = b.getDimensions();
  indexType q = 0;
  indexType Clen = dimsC.getElementCount();
  single *ptrA = (single *)a.getDataPointer();
  single *ptrB = (single *)b.getDataPointer();
  void *Cp = new_with_exception<single>(Clen * 2, false);
  single *C = (single *)Cp;
  singlecomplex *Az = reinterpret_cast<singlecomplex *>(ptrA);
  singlecomplex *Bz = reinterpret_cast<singlecomplex *>(ptrB);
  singlecomplex *Cz = reinterpret_cast<singlecomplex *>(C);
  if (reverse) {
    for (indexType i = 0; i < dimsC.getRows(); i++) {
      for (indexType j = 0; j < dimsC.getColumns(); j++) {
        indexType m = i + j * a.getDimensions().getRows();
        Cz[m] = Bz[m] - Az[q];
      }
      q++;
    }
  } else {
    for (indexType i = 0; i < dimsC.getRows(); i++) {
      for (indexType j = 0; j < dimsC.getColumns(); j++) {
        indexType m = i + j * a.getDimensions().getRows();
        Cz[m] = Az[q] - Bz[m];
      }
      q++;
    }
  }
  return ArrayOf(NLS_SCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf single_vector_column_subtraction(const ArrayOf &a,
                                                const ArrayOf &b,
                                                bool reverse = false) {
  const single *ptrA = (const single *)a.getDataPointer();
  const single *ptrB = (const single *)b.getDataPointer();
  Dimensions dimsC = b.getDimensions();
  indexType Clen = dimsC.getElementCount();
  void *Cp = new_with_exception<single>(Clen, false);
  single *C = (single *)Cp;
  if (reverse) {
    for (indexType i = 0; i < dimsC.getRows(); i++) {
      for (indexType j = 0; j < dimsC.getColumns(); j++) {
        indexType m = i + j * b.getDimensions().getRows();
        C[m] = ptrB[m] - ptrA[j];
      }
    }
  } else {
    for (indexType i = 0; i < dimsC.getRows(); i++) {
      for (indexType j = 0; j < dimsC.getColumns(); j++) {
        indexType m = i + j * b.getDimensions().getRows();
        C[m] = ptrA[j] - ptrB[m];
      }
    }
  }
  return ArrayOf(NLS_SINGLE, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf singlecomplex_vector_column_subtraction(const ArrayOf &a,
                                                       const ArrayOf &b,
                                                       bool reverse = false) {
  indexType q = 0;
  Dimensions dimsC = b.getDimensions();
  indexType Clen = dimsC.getElementCount();
  single *ptrA = (single *)a.getDataPointer();
  single *ptrB = (single *)b.getDataPointer();
  void *Cp = new_with_exception<single>(Clen * 2, false);
  single *C = (single *)Cp;
  singlecomplex *Az = reinterpret_cast<singlecomplex *>(ptrA);
  singlecomplex *Bz = reinterpret_cast<singlecomplex *>(ptrB);
  singlecomplex *Cz = reinterpret_cast<singlecomplex *>(C);
  if (reverse) {
    for (indexType i = 0; i < dimsC.getRows(); i++) {
      for (indexType j = 0; j < dimsC.getColumns(); j++) {
        indexType m = i + j * b.getDimensions().getRows();
        Cz[m] = Bz[m] - Az[j];
      }
    }
  } else {
    for (indexType i = 0; i < dimsC.getRows(); i++) {
      for (indexType j = 0; j < dimsC.getColumns(); j++) {
        indexType m = i + j * b.getDimensions().getRows();
        Cz[m] = Az[j] - Bz[m];
      }
    }
  }
  return ArrayOf(NLS_SCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
ArrayOf single_subtraction(ArrayOf a, ArrayOf b) {
  void *Cp = nullptr;
  if (a.isScalar() && b.isScalar()) {
    single res = (a.getContentAsSingleScalar() - b.getContentAsSingleScalar());
    return ArrayOf::singleConstructor(res);
  }
  Dimensions dimsA = a.getDimensions();
  Dimensions dimsB = b.getDimensions();
  Dimensions dimsC;
  if (a.isEmpty() || b.isEmpty()) {
    if (a.isScalar() || b.isScalar()) {
      if (a.isScalar()) {
        return ArrayOf(b);
      } else {
        return ArrayOf(a);
      }
    } else {
      if (!(SameSizeCheck(dimsA, dimsB))) {
        throw Exception(
            _W("Size mismatch on arguments to arithmetic operator ") + L"-");
      }
      return ArrayOf(b);
    }
  }
  if (SameSizeCheck(dimsA, dimsB)) {
    return single_matrix_matrix_subtraction(a, b);
  } else {
    if (a.isScalar() || b.isScalar()) {
      if (a.isScalar()) {
        return single_scalar_matrix_subtraction(a, b);
      } else {
        // b.isScalar()
        return single_scalar_matrix_subtraction(b, a, true);
      }
    } else {
      if (a.isVector() || b.isVector()) {
        if (a.isRowVector() && b.isColumnVector()) {
          dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                             std::max(dimsA.getMax(), dimsB.getMax()));
          indexType Clen = dimsC.getElementCount();
          Cp = new_with_exception<single>(Clen, false);
          single_vector_subtraction(
              (single *)Cp, (const single *)a.getDataPointer(),
              dimsA.getElementCount(), (const single *)b.getDataPointer(),
              dimsB.getElementCount());
        } else if (a.isColumnVector() && b.isRowVector()) {
          dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                             std::max(dimsA.getMax(), dimsB.getMax()));
          indexType Clen = dimsC.getElementCount();
          Cp = new_with_exception<single>(Clen, false);
          single_vector_subtraction(
              (single *)Cp, (const single *)b.getDataPointer(),
              dimsB.getElementCount(), (const single *)a.getDataPointer(),
              dimsA.getElementCount(), true);
        } else if ((a.isRowVector() && b.isRowVector()) ||
                   (a.isColumnVector() && b.isColumnVector())) {
          throw Exception(
              _W("Size mismatch on arguments to arithmetic operator ") + L"-");
        } else {
          const single *ptrA = (const single *)a.getDataPointer();
          const single *ptrB = (const single *)b.getDataPointer();

          if (dimsA[0] == dimsB[0]) {
            if (a.isVector()) {
              return single_vector_matrix_subtraction(a, b);
            } else {
              return single_vector_matrix_subtraction(b, a, true);
            }
          } else if (dimsA[1] == dimsB[1]) {
            if (a.isVector()) {
              return single_vector_column_subtraction(a, b);
            } else {
              return single_vector_column_subtraction(b, a, true);
            }
          } else {
            throw Exception(
                _W("Size mismatch on arguments to arithmetic operator ") +
                L"-");
          }
        }
      } else {
        throw Exception(
            _W("Size mismatch on arguments to arithmetic operator ") + L"-");
      }
    }
  }
  return ArrayOf(NLS_SINGLE, dimsC, Cp, false);
}
//=============================================================================
ArrayOf scomplex_subtraction(ArrayOf a, ArrayOf b) {
  a.promoteType(NLS_SCOMPLEX);
  b.promoteType(NLS_SCOMPLEX);
  void *Cp = nullptr;
  if (a.isScalar() && b.isScalar()) {
    singlecomplex ca = a.getContentAsSingleComplexScalar();
    singlecomplex cb = b.getContentAsSingleComplexScalar();
    singlecomplex res = ca - cb;
    return ArrayOf::complexConstructor(res.real(), res.imag());
  }
  Dimensions dimsA = a.getDimensions();
  Dimensions dimsB = b.getDimensions();
  Dimensions dimsC;
  if (a.isEmpty() || b.isEmpty()) {
    if (a.isScalar() || b.isScalar()) {
      if (a.isScalar()) {
        return ArrayOf(b);
      } else {
        return ArrayOf(a);
      }
    } else {
      if (!(SameSizeCheck(dimsA, dimsB))) {
        throw Exception(
            _W("Size mismatch on arguments to arithmetic operator ") + L"-");
      }
      return ArrayOf(b);
    }
  }
  if (SameSizeCheck(dimsA, dimsB)) {
    return singlecomplex_matrix_matrix_subtraction(a, b);
  } else {
    if (a.isScalar() || b.isScalar()) {
      if (a.isScalar()) {
        return singlecomplex_scalar_matrix_subtraction(a, b);
      } else {
        // b.isScalar()
        return singlecomplex_scalar_matrix_subtraction(b, a, true);
      }
    } else {
      if (a.isVector() || b.isVector()) {
        if (a.isRowVector() && b.isColumnVector()) {
          dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                             std::max(dimsA.getMax(), dimsB.getMax()));
          indexType Clen = dimsC.getElementCount();
          Cp = new_with_exception<single>(Clen * 2, false);
          singlecomplex_vector_subtraction(
              (single *)Cp, (single *)a.getDataPointer(),
              dimsA.getElementCount(), (single *)b.getDataPointer(),
              dimsB.getElementCount());
        } else if (a.isColumnVector() && b.isRowVector()) {
          dimsC = Dimensions(std::min(dimsA.getMax(), dimsB.getMax()),
                             std::max(dimsA.getMax(), dimsB.getMax()));
          indexType Clen = dimsC.getElementCount();
          Cp = new_with_exception<single>(Clen * 2, false);
          singlecomplex_vector_subtraction(
              (single *)Cp, (single *)b.getDataPointer(),
              dimsB.getElementCount(), (single *)a.getDataPointer(),
              dimsA.getElementCount(), true);
        } else if ((a.isRowVector() && b.isRowVector()) ||
                   (a.isColumnVector() && b.isColumnVector())) {
          throw Exception(
              _W("Size mismatch on arguments to arithmetic operator ") + L"-");
        } else {
          single *ptrA = (single *)a.getDataPointer();
          single *ptrB = (single *)b.getDataPointer();

          if (dimsA[0] == dimsB[0]) {
            if (a.isVector()) {
              return singlecomplex_vector_matrix_subtraction(a, b);
            } else {
              return singlecomplex_vector_matrix_subtraction(b, a, true);
            }
          } else if (dimsA[1] == dimsB[1]) {
            if (a.isVector()) {
              return singlecomplex_vector_column_subtraction(a, b);
            } else {
              return singlecomplex_vector_column_subtraction(b, a, true);
            }
          } else {
            throw Exception(
                _W("Size mismatch on arguments to arithmetic operator ") +
                L"-");
          }
        }
      } else {
        throw Exception(
            _W("Size mismatch on arguments to arithmetic operator ") + L"-");
      }
    }
  }
  return ArrayOf(NLS_SCOMPLEX, dimsC, Cp, false);
}
//=============================================================================
ArrayOf single_minus_single(ArrayOf a, ArrayOf b) {
  if (a.isComplex() || b.isComplex()) {
    ArrayOf res = scomplex_subtraction(a, b);
    if (res.allReal()) {
      res.promoteType(NLS_SINGLE);
    }
    return res;
  }
  return single_subtraction(a, b);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
