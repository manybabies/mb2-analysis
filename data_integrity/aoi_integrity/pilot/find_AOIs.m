v = VideoReader('FAM_LL_no_outcome_1200x900.mp4');

frame = read(v,1);

imwrite(frame, 'stim.png')

% tunnel diameter
d = 66;

% screen dimension
[screenHeight, screenWidth, ~] = size(frame);

% coordinates for left tunnel exit
L_left_X = 304;
L_left_Y = 618;

L_right_X = 370;
L_right_Y = 650;

L_top_X = 328;
L_top_Y = 586;

L_bottom_X = 339;
L_bottom_Y = 678;

% coordinates for right tunnel exit
R_left_X = 832;
R_left_Y = 650;

R_right_X = 896;
R_right_Y = 618;

R_top_X = 873;
R_top_Y = 586;

R_bottom_X = 867;
R_bottom_Y = 676;

% Left bounding box coordinates, width, height

% top left
l_x_min = (L_left_X - d) / screenWidth; % 0.1983
l_y_min = (L_top_Y - d) / screenHeight; % 0.5778

% bottom right
l_x_max = (L_right_X + d) / screenWidth; % 0.3633
l_y_max = (L_bottom_Y + d) / screenHeight; % 0.8267


w_L = (L_right_X + d - (L_left_X - d)) / screenWidth;
h_L = (L_bottom_Y + d - (L_top_Y - d)) / screenHeight;

% Right xy coordinates, width, height

% top left
r_x_min = (R_left_X - d) / screenWidth; % 0.6383
r_y_min = (R_top_Y - d) / screenHeight; % 0.5778

% bottom right
r_x_max = (R_right_X + d) / screenWidth; % 0.8017
r_y_max = (R_bottom_Y + d) / screenHeight; % 0.8244

w_R = (R_right_X + d - (R_left_X - d)) / screenWidth;
h_R = (R_bottom_Y + d - (R_top_Y - d)) / screenHeight;

% draw rectangles
imshow(frame)
hold on;
rectangle('Position', [l_x_min, l_y_min, w_L, h_L], 'Edgecolor', 'r', 'Curvature',0.2, 'LineWidth', 3)
hold on;
rectangle('Position', [r_y_min,r_y_min,w_R,h_R], 'Edgecolor', 'r', 'Curvature',0.2, 'LineWidth', 3)



