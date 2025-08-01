%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('zip'), -3);
assert_isequal(nargout('zip'), -1);
%=============================================================================
builderFile = [nelsonroot(),'/module_skeleton/builder.m'];
if ~isfile(builderFile)
  return
end
%=============================================================================
TMPDIR = tempdir()
if ismac()
  TMPDIR = ['/private', TMPDIR];
end
%=============================================================================
DEST_1 = [TMPDIR, 'zip_test_1.zip'];
cd([nelsonroot(), '/module_skeleton'])
R = zip(DEST_1, '*.m');
REF1 = {'builder.m'};
REF2 = {'builder.m', 'loader.m'};
if length(R) == 2
  assert_isequal(R, REF2);
else
  assert_isequal(R, REF1);
end
assert_istrue(isfile(DEST_1));
%=============================================================================
DEST_2 = [TMPDIR, 'zip_test_2.zip'];
cd([nelsonroot(), '/module_skeleton'])
R = zip(DEST_2, [nelsonroot(), '/module_skeleton/*.m']);
REF1 = {'builder.m'};
REF2 = {'builder.m', 'loader.m'};
if length(R) == 2
  assert_isequal(R, REF2);
else
  assert_isequal(R, REF1);
end
assert_istrue(isfile(DEST_2));
%=============================================================================
temp_dest = [TMPDIR, createGUID()];
mkdir(temp_dest);
cd(temp_dest);
R1 = unzip(DEST_1);
REF1 = {[temp_dest, '/builder.m']};
REF2 = {[temp_dest, '/builder.m'] , [temp_dest, '/loader.m']};
if length(R1) == 2
  assert_isequal(R1, REF2);
else
  assert_isequal(R1, REF1);
end
cd(tempdir());
rmdir(temp_dest, 's');
%=============================================================================
temp_dest = [TMPDIR, createGUID()];
mkdir(temp_dest);
cd(temp_dest);
R2 = unzip(DEST_2);
REF1 = {[temp_dest, '/builder.m']};
REF2 = {[temp_dest, '/builder.m'] , [temp_dest, '/loader.m']};
if length(R2) == 2
  assert_isequal(R2, REF2);
else
  assert_isequal(R2, REF1);
end
cd(tempdir());
rmdir(temp_dest, 's');
%=============================================================================
DEST_3 = [TMPDIR, 'zip_test_3.zip'];
R3 = zip(DEST_3, [nelsonroot(), '/module_skeleton']);
REF = {'module_skeleton/CHANGELOG.md', ...                       
'module_skeleton/COPYING-BSD', ...                        
'module_skeleton/LICENSE.md', ...                         
'module_skeleton/README.md', ...                          
'module_skeleton/builder.m', ...                          
'module_skeleton/builtin/', ...                           
'module_skeleton/builtin/builder.m', ...                  
'module_skeleton/builtin/cpp/', ...                       
'module_skeleton/builtin/cpp/cpp_sumBuiltin.cpp', ...     
'module_skeleton/builtin/include/', ...                   
'module_skeleton/builtin/include/cpp_sumBuiltin.hpp', ... 
'module_skeleton/etc/', ...                               
'module_skeleton/etc/finish.m', ...                       
'module_skeleton/etc/startup.m', ...                      
'module_skeleton/functions/', ...                         
'module_skeleton/functions/macro_sum.m', ...              
'module_skeleton/help/', ...                              
'module_skeleton/help/en_US/', ...                        
'module_skeleton/help/en_US/images/', ...                 
'module_skeleton/help/en_US/images/banner_nelson.png', ...
'module_skeleton/help/en_US/images/fibonacci.png', ...    
'module_skeleton/help/en_US/xml/', ...                    
'module_skeleton/help/en_US/xml/chapter.xml', ...         
'module_skeleton/help/en_US/xml/cpp_sum.xml', ...         
'module_skeleton/help/en_US/xml/nelson_sum.xml', ...      
'module_skeleton/module.json', ...                        
'module_skeleton/src/', ...                               
'module_skeleton/src/builder.m', ...                      
'module_skeleton/src/cpp/', ...                           
'module_skeleton/src/cpp/cpp_sum.cpp', ...                
'module_skeleton/src/include/', ...                       
'module_skeleton/src/include/cpp_sum.hpp', ...            
'module_skeleton/tests/', ...                             
'module_skeleton/tests/bench_cpp_sum.m', ...              
'module_skeleton/tests/bench_macro_sum.m', ...            
'module_skeleton/tests/test_cpp_sum.m', ...               
'module_skeleton/tests/test_macro_sum.m'};
assert_istrue(isfile(DEST_3));
info = dir(DEST_3);
assert_istrue(info.bytes > 0);
assert_istrue(any(contains(REF, R3)));
%=============================================================================
DEST_4 = [TMPDIR, 'zip_test_4.zip'];
R4 = zip(DEST_4, [nelsonroot(), '/module_skeleton/']);
REF = {                        
'CHANGELOG.md', ...                       
'COPYING-BSD', ...                        
'LICENSE.md', ...                         
'README.md', ...                          
'builder.m', ...                          
'builtin/', ...                           
'builtin/builder.m', ...                  
'builtin/cpp/', ...                       
'builtin/cpp/cpp_sumBuiltin.cpp', ...     
'builtin/include/', ...                   
'builtin/include/cpp_sumBuiltin.hpp', ... 
'etc/', ...                               
'etc/finish.m', ...                       
'etc/startup.m', ...                      
'functions/', ...                         
'functions/macro_sum.m', ...              
'help/', ...                              
'help/en_US/', ...                        
'help/en_US/images/', ...                 
'help/en_US/images/banner_nelson.png', ...
'help/en_US/images/fibonacci.png', ...    
'help/en_US/xml/', ...                    
'help/en_US/xml/chapter.xml', ...         
'help/en_US/xml/cpp_sum.xml', ...         
'help/en_US/xml/nelson_sum.xml', ...      
'module.json', ...                        
'src/', ...                               
'src/builder.m', ...                      
'src/cpp/', ...                           
'src/cpp/cpp_sum.cpp', ...                
'src/include/', ...                       
'src/include/cpp_sum.hpp', ...            
'tests/', ...                             
'tests/bench_cpp_sum.m', ...              
'tests/bench_macro_sum.m', ...            
'tests/test_cpp_sum.m', ...               
'tests/test_macro_sum.m'};
assert_istrue(isfile(DEST_4));
info = dir(DEST_4);
assert_istrue(info.bytes > 0);
assert_istrue(any(contains(REF, R4)));
%=============================================================================
DEST_5 = [TMPDIR, 'zip_test_5.zip'];
R5 = zip(DEST_5, [nelsonroot(), '/module_skeleton/.']);
REF = {'CHANGELOG.md', ...                      
  'COPYING-BSD', ...                       
  'LICENSE.md', ...                        
  'README.md', ...                         
  'builder.m', ...                         
  'builtin/', ...                          
  'builtin/builder.m', ...                 
  'builtin/cpp/', ...                      
  'builtin/cpp/cpp_sumBuiltin.cpp', ...    
  'builtin/include/', ...                  
  'builtin/include/cpp_sumBuiltin.hpp', ...
  'etc/', ...                              
  'etc/finish.m', ...                      
  'etc/startup.m', ...                     
  'functions/', ...                        
  'functions/macro_sum.m', ...             
  'help/', ...                             
  'help/en_US/', ...                       
  'help/en_US/images/', ...                
  'help/en_US/images/banner_nelson.png', ...
  'help/en_US/images/fibonacci.png', ...   
  'help/en_US/xml/', ...                   
  'help/en_US/xml/chapter.xml', ...        
  'help/en_US/xml/cpp_sum.xml', ...        
  'help/en_US/xml/nelson_sum.xml', ...     
  'module.json', ...                       
  'src/', ...                              
  'src/builder.m', ...                     
  'src/cpp/', ...                          
  'src/cpp/cpp_sum.cpp', ...               
  'src/include/', ...                      
  'src/include/cpp_sum.hpp', ...           
  'tests/', ...                            
  'tests/bench_cpp_sum.m', ...             
  'tests/bench_macro_sum.m', ...           
  'tests/test_cpp_sum.m', ...              
  'tests/test_macro_sum.m'};
assert_istrue(isfile(DEST_5));
info = dir(DEST_5);
assert_istrue(info.bytes > 0);
assert_istrue(any(contains(REF, R5)));
%=============================================================================
cmd = 'R = zip(DEST_1, ''*.m'', [nelsonroot(), ''/modules_skeleton''])';
assert_checkerror(cmd, _('Invalid root path.'));
%=============================================================================
