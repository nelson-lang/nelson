%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ctrbf(varargin)
  % Compute controllability staircase form
  % [Abar, Bbar, Cbar, T, k] = ctrbf(A, B, C)
  % [Abar, Bbar, Cbar, T, k] = ctrbf(A, B, C, tol)
  
  % Check input and output arguments
  narginchk(3, 4);
  nargoutchk(0, 5);
  
  % Extract input matrices
  A = varargin{1};
  B = varargin{2};
  C = varargin{3};
  
  % Set tolerance based on input or default
  if nargin == 3
    tol = size(A, 1) * norm(A, 1) * eps;
  else
    tol = varargin{4};
  end
  
  % Initialize variables
  P_t_minus_1 = eye(size(A, 1));
  A_current = A;
  B_current = B;
  ro_j_minus_1 = size(B, 2);
  delta_j_minus_1 = 0;
  sigma_j_minus_1 = size(A, 1);
  k_values = zeros(1, size(A, 1));
  
  % Main loop for controllability staircase form
  for j = 1:size(A, 1)
    % Singular value decomposition of B_current
    [U_j, S_j, void] = svd(B_current);
    [row_S_j, void] = size(S_j);
    
    % Rotate the singular vectors
    rotation_matrix = rot90(eye(row_S_j), 1);
    U_j = U_j * rotation_matrix;
    
    % Update B_current using the rotated singular vectors
    B_current = U_j' * B_current;
    
    % Compute the rank of B_current
    ro_j = rank(B_current, tol);
    [row_B_current, void] = size(B_current);
    
    % Update variables
    sigma_j = row_B_current - ro_j;
    sigma_j_minus_1 = sigma_j;
    k_values(j) = ro_j;
    
    % Check termination conditions
    if (ro_j == 0 || sigma_j == 0)
      break;
    end
    
    % Update A_current and B_current based on rotated singular vectors
    A_and_B_rotated = U_j' * A_current * U_j;
    A_current = A_and_B_rotated(1:sigma_j, 1:sigma_j);
    B_current = A_and_B_rotated(1:sigma_j, sigma_j + 1:sigma_j + ro_j);
    
    % Update P_t_minus_1 matrix
    [row_U_j, col_U_j] = size(U_j);
    P_t = P_t_minus_1 * [U_j zeros(row_U_j, delta_j_minus_1); zeros(delta_j_minus_1, col_U_j) eye(delta_j_minus_1)];
    P_t_minus_1 = P_t;
    
    % Update delta_j_minus_1
    delta_j = delta_j_minus_1 + ro_j;
    delta_j_minus_1 = delta_j;
  end
  
  % Compute the controllability transformation matrix T
  T = P_t_minus_1';
  
  % Transform the system matrices to controllability form
  A_bar = T * A * T';
  B_bar = T * B;
  C_bar = C * T';
  
  % Output results
  varargout{1} = A_bar;
  varargout{2} = B_bar;
  varargout{3} = C_bar;
  varargout{4} = T;
  varargout{5} = k_values;
end
