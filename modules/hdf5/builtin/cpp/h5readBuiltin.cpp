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
#include "h5readBuiltin.hpp"
#include "Error.hpp"
#include "h5Read.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// data = h5read(filename, datasetname)
// data = h5read(filename, datasetname, start, count)
// data = h5read(filename, datasetname, start, count, stride)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5readBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    boost::container::vector<double> start;
    boost::container::vector<double> count;
    boost::container::vector<double> stride;
    std::wstring filename;
    std::wstring datasetname;

	switch (argIn.size()) {
    case 2: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        filename = param1.getContentAsWideString();
        datasetname = param2.getContentAsWideString();
    } break;
    case 4: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        ArrayOf param4 = argIn[3];
        filename = param1.getContentAsWideString();
        datasetname = param2.getContentAsWideString();
        if (param3.getDataClass() != NLS_DOUBLE) {
            Error(_W("double positive values expected."));
        }
        if (param4.getDataClass() != NLS_DOUBLE) {
            Error(_W("double positive values expected."));
        }
        if (!param3.getDimensions().equals(param4.getDimensions())) {
            Error(_W("Same dimensions expected."));
        }
        indexType nbElements = param3.getDimensions().getElementCount();
        start.reserve(nbElements);
        count.reserve(nbElements);
        double* ptrParam3 = (double*)param3.getDataPointer();
        double* ptrParam4 = (double*)param4.getDataPointer();
		for (indexType k = 0; k < nbElements; k++) {
            start.push_back(ptrParam3[k]);
            count.push_back(ptrParam4[k]);
		}
	} break;
    case 5: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        ArrayOf param4 = argIn[3];
        ArrayOf param5 = argIn[4];
        filename = param1.getContentAsWideString();
        datasetname = param2.getContentAsWideString();
        if (param3.getDataClass() != NLS_DOUBLE) {
            Error(_W("double positive values expected."));
        }
        if (param4.getDataClass() != NLS_DOUBLE) {
            Error(_W("double positive values expected."));
        }
        if (param5.getDataClass() != NLS_DOUBLE) {
            Error(_W("double positive values expected."));
        }
        if (!param3.getDimensions().equals(param4.getDimensions())) {
            Error(_W("Same dimensions expected."));
        }
        if (!param4.getDimensions().equals(param5.getDimensions())) {
            Error(_W("Same dimensions expected."));
        }
        indexType nbElements = param3.getDimensions().getElementCount();
        start.reserve(nbElements);
        count.reserve(nbElements);
        stride.reserve(nbElements);
        double* ptrParam3 = (double*)param3.getDataPointer();
        double* ptrParam4 = (double*)param4.getDataPointer();
        double* ptrParam5 = (double*)param5.getDataPointer();
		for (indexType k = 0; k < nbElements; k++) {
            start.push_back(ptrParam3[k]);
            count.push_back(ptrParam4[k]);
            stride.push_back(ptrParam5[k]);
        }
    } break;
    default: { 
		Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
	} break;
	}
    retval.push_back(h5Read(filename, datasetname, start, count, stride));
    return retval;
}
//=============================================================================
