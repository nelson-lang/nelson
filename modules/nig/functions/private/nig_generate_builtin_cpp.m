%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_generate_builtin_cpp(NIG_FUNCTION)
  txt = nig_header_license();
  txt{end + 1} = ['// Generated by Nelson Interface Generator ', nig_version()];
  txt{end + 1} = '//=============================================================================';
  CppBuiltinName = [NIG_FUNCTION.NELSON_NAME, 'Builtin'];
  filenameHeaderBuiltin = [CppBuiltinName, '.hpp'];
  txt{end + 1} = '#include <algorithm>';
  txt{end + 1} = '#include "Error.hpp"';
  txt{end + 1} = '#include "i18n.hpp"';
  txt{end + 1} = ['#include "', filenameHeaderBuiltin, '"'];
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = 'using namespace Nelson;';
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = '#ifdef __cplusplus';
  txt{end + 1} = 'extern "C"';
  txt{end + 1} = '{';
  txt{end + 1} = '#endif';
  txt{end + 1} = ['extern ', nig_language_function_declaration(NIG_FUNCTION), ';'];
  txt{end + 1} = '#ifdef __cplusplus';
  txt{end + 1} = '}';
  txt{end + 1} = '#endif';
  txt{end + 1} = '//=============================================================================';
  txt{end + 1} = ['// ', nig_nelson_function_prototype(NIG_FUNCTION)];
  txt{end + 1} = '//=============================================================================';
  if isfield(NIG_FUNCTION, 'NELSON_NAMESPACE')
    txt{end + 1} = ['ArrayOfVector Nelson::', NIG_FUNCTION.NELSON_NAMESPACE, 'Gateway::', NIG_FUNCTION.NELSON_NAME, 'Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)'];
  else
    txt{end + 1} = ['ArrayOfVector Nelson::', NIG_FUNCTION.NELSON_NAME, 'Builtin(int nLhs, const ArrayOfVector& argIn)'];
  end
  txt{end + 1} = '{';
  txt{end + 1} = '    ArrayOfVector retval(nLhs);';
  txt{end + 1} = ['    nargoutcheck(nLhs, 0, ', int2str(nig_nargout(NIG_FUNCTION)) ,')'];
  txt{end + 1} = ['    nargincheck(argIn, ', int2str(nig_nargin(NIG_FUNCTION)), ', ', int2str(nig_nargin(NIG_FUNCTION)), ')'];
  txt{end + 1} = '';
  
  % INPUT VARIABLES DEFINITION
  res = nig_get_input_variables_definition(NIG_FUNCTION);
  txt = [txt; res];
  txt{end + 1} = '';
  
  % INPUT/OUTPUT VARIABLES DEFINITION
  [res] = nig_get_in_out_variables_definition(NIG_FUNCTION);
  txt = [txt; res];
  txt{end + 1} = '';
  
  % LOCAL VARIABLES DEFINITION
  txt = [txt; nig_get_local_variables_definition(NIG_FUNCTION)];
  txt{end + 1} = '';
  
  % OUTPUT VARIABLES DEFINITION
  txt = [txt; nig_get_output_variables_definition(NIG_FUNCTION)];
  txt{end + 1} = '';
  
  % CHECK INPUT VARIABLES DIMENSIONS
  [res] = nig_check_dimensions_input_variables_definition(NIG_FUNCTION);
  txt = [txt; res];
  txt{end + 1} = '';
  
  % CALL EXTERN FUNCTION
  txt = [txt ; nig_call_function(NIG_FUNCTION)];
  txt{end + 1} = '';
  
  % ASSIGN OUTPUT
  txt = [txt ; nig_assign_output_variables(NIG_FUNCTION)];
  txt{end + 1} = '';
  
  txt{end + 1} = '    return retval;';
  txt{end + 1} = '}';
  txt{end + 1} = '//=============================================================================';
  
end
%=============================================================================
