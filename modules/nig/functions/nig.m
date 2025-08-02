%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nig(varargin)
  if nargin ~= 2
    error(_('Wrong number of input arguments.'));
  end
  NIG_FUNCTIONS = varargin{1};
  NIG_DESTINATION = varargin{2};
  if ~isstruct(NIG_FUNCTIONS)
    error(_('struct expected.'));
  end
  if ~isdir(NIG_DESTINATION)
    error(_('an existing directory expected.'));
  end
  try
    have_module_subdirectories =  isdir([NIG_DESTINATION, '/builtin/cpp']) && isdir([NIG_DESTINATION, '/builtin/include']);
    len = length(NIG_FUNCTIONS);
    for k = [1:len]
      if have_module_subdirectories
        filenameCppBuiltin = [NIG_DESTINATION, '/builtin/cpp/', NIG_FUNCTIONS(k).NELSON_NAME, 'Builtin.cpp'];
        filenameHeaderBuiltin = [NIG_DESTINATION, '/builtin/include/', NIG_FUNCTIONS(k).NELSON_NAME, 'Builtin.hpp'];
      else
        filenameCppBuiltin = [NIG_DESTINATION, '/', NIG_FUNCTIONS(k).NELSON_NAME, 'Builtin.cpp'];
        filenameHeaderBuiltin = [NIG_DESTINATION, '/', NIG_FUNCTIONS(k).NELSON_NAME, 'Builtin.hpp'];
      end
      nig_txt_to_file(nig_generate_builtin_hpp(NIG_FUNCTIONS(k)), filenameHeaderBuiltin);
      nig_txt_to_file(nig_generate_builtin_cpp(NIG_FUNCTIONS(k)), filenameCppBuiltin);
    end
    if have_module_subdirectories
      filenameCppGateway = [NIG_DESTINATION, '/builtin/cpp/Gateway.cpp'];
    else
      filenameCppGateway = [NIG_DESTINATION, '/', 'Gateway.cpp'];
    end
    nig_txt_to_file(nig_generate_gateway_cpp(NIG_FUNCTIONS), filenameCppGateway);
  catch
    error_struct = lasterror();
    error(error_struct);
  end
end
%=============================================================================
