%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function savebin(varargin)
  % savebin(filename, 'var1','varN')
  % savebin(filename)
  
  version_nlbfile = 1;
  tag_nlbfile = 96;
  
  if (nargin == 0)
    error(_('Wrong number of input arguments.'))
  end
  filename = varargin{1};
  if nargin == 1
    names = who('caller');
    for k = 1:length(names)
      mtx{k} = acquirevar('caller', names{k})
    end
  else
    names = {};
    for k = 2:nargin()
      if (~ischar(varargin{k}))
        error(_('Wrong type for argument #1. string expected.'))
      end
      names{k - 1} = varargin{k};
      mtx{k - 1} = acquirevar('caller', names{k - 1});
    end
  end
  fw = fopen(filename,'wb');
  if (fw == -1)
    error([_('Impossible to open: '), filename]);
  end
  
  fwrite(fw, tag_nlbfile, 'int', 'l');
  fwrite(fw, version_nlbfile, 'int', 'l');
  
  nbVariables = length(names);
  fwrite(fw, nbVariables, 'int', 'l');
  for k = 1:nbVariables
    fwrite(fw, length(names{k}), 'int', 'l')
    fwrite(fw, names{k}, 'double', 'l');
  end
  
  for k = 1:nbVariables
    save_data(fw, mtx{k});
  end
  
  fclose(fw);
end
%=============================================================================
function save_data(fd, data)
  if isclass(data)
    function_save = 'class_save_data';
  else
    function_save = [class(data), '_save_data'];
  end
  feval(function_save, fd, data)
end
%=============================================================================
function sparse_save_data(fd, data)
  [I, J, V, m, n, nzmax] = IJV(data);
  save_data(fd, I);
  save_data(fd, J);
  save_data(fd, V);
  save_data(fd, m);
  save_data(fd, n);
  save_data(fd, nzmax);
end
%=============================================================================
function sparsedouble_save_data(fd, data)
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  sparse_save_data(fd, data);
end
%=============================================================================
function sparselogical_save_data(fd, data)
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  sparse_save_data(fd, data)
end
%=============================================================================
function ndarraygeneric_save_data(fd, data)
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  dim = size(data);
  len = length(dim);
  fwrite(fd, len, 'double', 'l');
  fwrite(fd, dim, 'double', 'l');
  
  if ~isempty(data)
    d = reshape(data, 1, []);
    save_data(fd, d);
  end
end
%=============================================================================
function ndarraylogical_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarrayuint8_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarrayint8_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarrayuint16_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarrayint16_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarrayuint32_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarrayint32_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarrayuint64_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarrayint64_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarraysingle_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarraydouble_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function ndarraychar_save_data(fd, data)
  ndarraygeneric_save_data(fd, data)
end
%=============================================================================
function generic_cell_save_data(fd, data)
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  dim = size(data);
  len = length(dim);
  fwrite(fd, len, 'double', 'l');
  fwrite(fd, dim, 'double', 'l');
  
  if ~isempty(data)
    prod = 1;
    for k = 1:len
      prod = prod * dim(k);
    end
    for k = 1:prod
      save_data(fd, data{k});
    end
  end
end
%=============================================================================
function ndarraycell_save_data(fd, data)
  generic_cell_save_data(fd, data)
end
%=============================================================================
function cell_save_data(fd, data)
  generic_cell_save_data(fd, data)
end
%=============================================================================
function string_save_data(fd, data)
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  dim = size(data);
  len = length(dim);
  fwrite(fd, len, 'double', 'l');
  fwrite(fd, dim, 'double', 'l');
  
  if ~isempty(data)
    prod = 1;
    for k = 1:len
      prod = prod * dim(k);
    end
    for k = 1:prod
      save_data(fd, data{k});
    end
  end
end
%=============================================================================
function struct_save_data(fd, data)
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  dim = size(data);
  len = length(dim);
  fwrite(fd, len, 'double', 'l');
  fwrite(fd, dim, 'double', 'l');
  names = fieldnames(data);
  save_data(fd, names);
  c = struct2cell(data);
  save_data(fd, c);
end
%=============================================================================
function generic_notcomplex_save_data(fd, data, typestr)
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  dim = size(data);
  len = length(dim);
  fwrite(fd, len, 'double', 'l');
  fwrite(fd, dim, 'double', 'l');
  
  if ~isempty(data)
    fwrite(fd, data, typestr, 'l');
  end
end
%=============================================================================
function uint8_save_data(fd, data)
  generic_notcomplex_save_data(fd, data, 'uint8');
end
%=============================================================================
function int8_save_data(fd, data)
  generic_notcomplex_save_data(fd, data, 'int8')
end
%=============================================================================
function uint16_save_data(fd, data)
  generic_notcomplex_save_data(fd, data, 'uint16')
end
%=============================================================================
function int16_save_data(fd, data)
  generic_notcomplex_save_data(fd, data, 'int16')
end
%=============================================================================
function uint32_save_data(fd, data)
  generic_notcomplex_save_data(fd, data, 'uint32')
end
%=============================================================================
function int32_save_data(fd, data)
  generic_notcomplex_save_data(fd, data, 'int32')
end
%=============================================================================
function uint64_save_data(fd, data)
  generic_notcomplex_save_data(fd, data, 'uint64')
end
%=============================================================================
function int64_save_data(fd, data)
  generic_notcomplex_save_data(fd, data, 'int64')
end
%=============================================================================
function generic_complex_save_data(fd, data, typestr)
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  dim = size(data);
  len = length(dim);
  fwrite(fd, len, 'double', 'l');
  fwrite(fd, dim, 'double', 'l');
  
  fwrite(fd, isreal(data), 'logical', 'l');
  if ~isempty(data)
    if (isreal(data))
      fwrite(fd, data, typestr, 'l');
    else
      fwrite(fd, real(data), typestr, 'l');
      fwrite(fd, imag(data), typestr, 'l');
    end
  end
end
%=============================================================================
function single_save_data(fd, data)
  generic_complex_save_data(fd, data, 'single')
end
%=============================================================================
function logical_save_data(fd, data)
  generic_notcomplex_save_data(fd, data, 'logical')
end
%=============================================================================
function double_save_data(fd, data)
  generic_complex_save_data(fd, data, 'double')
end
%=============================================================================
function char_save_data(fd, data)
  % unicode saved as uint32
  generic_notcomplex_save_data(fd, data, 'uint32')
end
%=============================================================================
function function_handle_save_data(fd, data)
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  f = func2str(data);
  char_save_data(fd, f);
end
%=============================================================================
function handle_save_data(fd, data)
  error(_('not yet implemented.'))
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  dim = size(data);
  len = length(dim);
  fwrite(fd, len, 'double', 'l');
  fwrite(fd, dim, 'double', 'l');
  
end
%=============================================================================
function generic_save_data(fd, data)
  error(_('not yet implemented.'))
  fwrite(fd, int64(typebin(class(data))), 'int64', 'l');
  dim = size(data);
  len = length(dim);
  fwrite(fd, len, 'double', 'l');
  fwrite(fd, dim, 'double', 'l');
  
end
%=============================================================================
function class_save_data(fd, data)
  classname = class(data);
  fwrite(fd, 2000, 'int64', 'l');
  dim = size(data);
  len = length(dim);
  fwrite(fd, len, 'double', 'l');
  fwrite(fd, dim, 'double', 'l');
  function_to_call = [classname,'_save_data'];
  feval(function_save, fd, data);
end
%=============================================================================
