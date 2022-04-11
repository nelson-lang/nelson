%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function loadbin(varargin)
  if (nargin == 0)
    error(_('Wrong number of input arguments.'))
  end
  S = {};
  filename = varargin{1};
  if ~isfile(filename)
    error(_('An existing filename expected.'));
  end
  fr = fopen(filename,'rb');
  version_nlbfile_expected = 1;
  tag_nlbfile_expected = 96;

  tag_nlbfile_currentfile = fread(fr, 1, 'int','l');
  if (tag_nlbfile_currentfile ~= tag_nlbfile_expected)
    fclose(fr);
    error(_('file format not a supported file.'));
  end

  version_nlbfile_currentfile = fread(fr, 1, 'int','l');
  if (version_nlbfile_currentfile ~= version_nlbfile_expected)
    fclose(fr);
    error(_('version not a supported file.'));
  end

  nbVariables = fread(fr, 1, 'int');
  varname = {};
  for k = 1:nbVariables
    nbChars = fread(fr, 1, 'int');
    ichars = fread(fr, nbChars, 'double');
    varname{k} = reshape(char(ichars), 1, []);
  end

  if nargin == 1
    for k = 1:nbVariables
      RES = load_data(fr);
      assignin('caller', varname{k}, RES);
    end
  else
    names_to_read = {};
    for k = 2:nargin()
      if (~ischar(varargin{k}))
        error(_('Wrong type for argument #1. string expected.'))
      end
      names_to_read{k - 1} = varargin{k};
      emptyCells = all(cellfun('isempty', strfind(varname, names_to_read{k - 1})));
      if (emptyCells)
        warning(['Variable '', names_to_read{k - 1}, '' not found.']);
        return
      end
    end
    for k = 1:nbVariables
      RES = load_data(fr);
      if (strcmp(varname{k}, names_to_read{k}) == 1)
        assignin('caller', varname{k}, RES);
      end
    end
  end

  fclose(fr);

end
%=============================================================================
function res = complex_load_data(fr, typestr)
  lendim = fread(fr, 1, 'double', 'l');
  dim = fread(fr, lendim, 'double', 'l');
  dim = reshape(dim, 1, []);
  prod = 1;
  for k = 1:lendim
    prod = prod * dim(k);
  end
  isReal = fread(fr, 1, 'logical', 'l');
  if (isReal)
    res = fread(fr, prod, typestr, 'l');
  else
    resReal = fread(fr, prod, typestr, 'l');
    resImag = fread(fr, prod, typestr, 'l');
    res = complex(resReal, resImag);
  end
  res = reshape(res, dim);
end
%=============================================================================
function res = double_load_data(fr)
  res = complex_load_data(fr, 'double');
end
%=============================================================================
function res = single_load_data(fr)
  res = complex_load_data(fr, 'single');
end
%=============================================================================
function res = sparsegeneric_load_data(fr)
  I = load_data(fr);
  J = load_data(fr);
  V = load_data(fr);
  m = load_data(fr);
  n = load_data(fr);
  nzmax = load_data(fr);
  res = sparse(I, J, V, m, n, nzmax);
end
%=============================================================================
function res = sparsedouble_load_data(fr)
  res = sparsegeneric_load_data(fr);
end
%=============================================================================
function res = sparselogical_load_data(fr)
  res = sparsegeneric_load_data(fr);
end
%=============================================================================
function res = ndarraygeneric_load_data(fr)
  lendim = fread(fr, 1, 'double', 'l');
  dim = fread(fr, lendim, 'double', 'l');
  dim = reshape(dim, 1, []);
  res = load_data(fr);
  res = reshape(res, dim);
end
%=============================================================================
function res = ndarraylogical_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarrayuint8_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarrayint8_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarrayuint16_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarrayint16_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarrayuint32_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarrayint32_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarrayuint64_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarrayint64_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarraysingle_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarraydouble_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = ndarraychar_load_data(fr)
  res = ndarraygeneric_load_data(fr);
end
%=============================================================================
function res = generic_cell_load_data(fr)
  lendim = fread(fr, 1, 'double', 'l');
  dim = fread(fr, lendim, 'double', 'l');
  dim = reshape(dim, 1, []);
  res = cell(dim);
  prod = 1;
  for k = 1:lendim
    prod = prod * dim(k);
  end
  for k = 1:prod
    res{k} = load_data(fr);
  end
  res = reshape(res, dim);
end
%=============================================================================
function res = ndarraycell_load_data(fr)
  res = generic_cell_load_data(fr);
end
%=============================================================================
function res = cell_load_data(fr);
  res = generic_cell_load_data(fr);
end
%=============================================================================
function res = string_load_data(fr);
  lendim = fread(fr, 1, 'double', 'l');
  dim = fread(fr, lendim, 'double', 'l');
  dim = reshape(dim, 1, []);
  res = strings(dim);
  prod = 1;
  for k = 1:lendim
    prod = prod * dim(k);
  end
  for k = 1:prod
    res{k} = load_data(fr);
  end
  res = reshape(res, dim);
end
%=============================================================================
function res = struct_load_data(fr)
  lendim = fread(fr, 1, 'double', 'l');
  dim = fread(fr, lendim, 'double', 'l');
  dim = reshape(dim, 1, []);
  names = load_data(fr);
  ce = load_data(fr);
  if isequal(dim, [1 1]) && isempty(names)
    res = struct();
  else
    if isempty(names)
      res = struct(zeros(dim));
    else
      restmp = cell2struct(ce, names);
      try
        res = reshape(restmp, dim);
      catch
        res = restmp;
      end
    end
  end
end
%=============================================================================
function res = generic_notcomplex_load_data(fr, typestr)
  lendim = fread(fr, 1, 'double', 'l');
  dim = fread(fr, lendim, 'double', 'l');
  dim = reshape(dim, 1, []);
  prod = 1;
  for k = 1:lendim
    prod = prod * dim(k);
  end
  res = fread(fr, prod, typestr, 'l');
  res = reshape(res, dim);
end
%=============================================================================
function res = logical_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'logical');
end
%=============================================================================
function res = uint8_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'uint8');
end
%=============================================================================
function res = int8_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'int8');
end
%=============================================================================
function res = uint16_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'uint16');
end
%=============================================================================
function res = int16_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'int16');
end
%=============================================================================
function res = uint32_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'uint32');
end
%=============================================================================
function res = int32_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'int32');
end
%=============================================================================
function res = uint64_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'uint64');
end
%=============================================================================
function res = int64_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'int64');
end
%=============================================================================
function res = char_load_data(fr)
  res = generic_notcomplex_load_data(fr, 'uint32');
  res = char(res);
end
%=============================================================================
function res = function_handle_load_data(fr)
  str = load_data(fr);
  res = str2func(str);
end
%=============================================================================
function res = handle_load_data(fr)
  error(_('not yet implemented.'));
end
%=============================================================================
function res = generic_load_data(fr)
  error(_('not yet implemented.'));
end
%=============================================================================
function res = class_load_data(fr)
  error(_('not yet implemented.'));
end
%=============================================================================
function res = load_data(fr)
  typenum = fread(fr, 1, 'int64', 'l');
  classstr = typeofbin(typenum);
  function_load = [classstr, '_load_data'];
  res = feval(function_load, fr);
end
%=============================================================================
