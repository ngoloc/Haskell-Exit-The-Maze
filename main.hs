import Data.String.Utils
import Graphics.GD

--Node r c
data Cell = Node Int Int

--a function to divide 2 integers
intDiv :: Int -> Int -> Int
intDiv a b = 
      let x = fromIntegral a
          y = fromIntegral b
      in truncate (x / y)

--icon size (width = height = 40)
icon_l = 40

--update 2D Mark Matrix - m_lst
update_level_2 (head:tail) c index_c
		| index_c < c = (head : update_level_2 tail c (index_c+1))
		| index_c == c = (True:tail)
		| otherwise = error "Bad index"

update_level_1 (head:tail) r c index_r
		| index_r < r = (head : update_level_1 tail r c (index_r+1))
		| index_r == r = ((update_level_2 head c 0):tail)
		| otherwise = error "Bad index"

mark m_lst r c = update_level_1 m_lst r c 0

--initialize 2D Mark Matrix - m_lst
init_m_c c = if c == 0 then [] else (False:init_m_c (c-1))
init_m_r r c = if r == 0 then [] else ((init_m_c c) : init_m_r (r-1) c)

init_m_lst maze = init_m_r h w
		  where h = length maze
		  	w = length (head maze)

--get neighbors from current cell, mark them in m_lst and update the backtrack list (b_lst)
findNeighbors (Node r c) m_lst maze = let (top, m_lst1, b_lst1) =
						if r-1 >= 0 && ((maze!!(r-1))!!c) /= 'x'
							    && (m_lst!!(r-1))!!c == False
						then ([(Node (r-1) c)], mark m_lst (r-1) c, [(r-1,c,r,c)]) else ([], m_lst,[])
			              in let (right, m_lst2, b_lst2) = 
						if c+1 < w && ((maze!!r)!!(c+1)) /= 'x'
							   && (m_lst!!r)!!(c+1) == False
						then ([(Node r (c+1))], mark m_lst1 r (c+1),[(r,c+1,r,c)]) else ([], m_lst1,[])
				      in let (bottom, m_lst3, b_lst3) = 
						if r+1 < h && ((maze!!(r+1))!!c) /= 'x'
							   && (m_lst!!(r+1))!!c == False
						then ([(Node (r+1) c)], mark m_lst2 (r+1) c,[(r+1,c,r,c)]) else ([], m_lst2,[])
				      in let (left, m_lst4, b_lst4) = 
						if c-1 >=0 && ((maze!!r)!!(c-1)) /= 'x'
							   && (m_lst!!r)!!(c-1) == False
						then ([(Node r (c-1))], mark m_lst3 r (c-1),[(r,c-1,r,c)]) else ([], m_lst3,[])
				      in (top ++ right ++ bottom ++ left, m_lst4, b_lst1 ++ b_lst2 ++ b_lst3 ++ b_lst4)
				      where h = length maze
					    w = length (head maze)

--deque and return the backtrack list
deque [] _ b_lst _ = b_lst

deque (s:tail) m_lst b_lst maze = let (neighbors,m_lst1,b_lst1) = findNeighbors s m_lst maze
		                  in deque (tail ++ neighbors) m_lst1 (b_lst ++ b_lst1) maze

--BFS current_cell maze mark_array backtrack_array hero_cell exit_cell
--    return the path from hero position to exit door
bfs h_cell maze = return (let q = [h_cell]
			  in let m_lst = init_m_lst maze
			  in let b_lst = deque q m_lst [] maze
			  in b_lst)

--read the maze and modify the image
draw_cell r c maze_img maze = do icon <- io_icon
	 		         copyRegion (0,0) (icon_l,icon_l) icon (icon_l*c,icon_l*r) maze_img
				 return (if cell_char == 's' then ((r,c),(-1,-1))
					 else if cell_char == 'e' then ((-1,-1),(r,c))
					      else ((-1,-1),(-1,-1)))
			      where cell_char = ((maze !! r) !! c)
				    io_icon = case cell_char of
						'x' -> loadPngFile "./images/wall-icon.png"
						otherwise -> loadPngFile "./images/floor-icon.png"
--return hero cell or exit cell if applicable
choose_cell cell new_cell = if new_cell == (-1,-1) then cell else new_cell  

--draw the maze, meanwhile identify the hero cell & exit cell
draw_maze r c maze_img maze h_cell x_cell = do (h_cell1,x_cell1) <- draw_cell r c maze_img maze
                                 	       if (c+1) >= w && (r+1) < h then draw_maze (r+1) 0 maze_img maze
											 (choose_cell h_cell h_cell1)
											 (choose_cell x_cell x_cell1)
				               else if (c+1) < w then draw_maze r (c+1) maze_img maze
										(choose_cell h_cell h_cell1)
										(choose_cell x_cell x_cell1)
				                    else return (h_cell, x_cell)
			      		    where h = length maze
				                  w = length (head maze)

--lookup the next cell to move, given the current cell and path list
look_up [] _ = (-1,-1)
look_up ((src_r,src_c,dest_r,dest_c):tail) (src_cell_r,src_cell_c) =
	if src_r == src_cell_r && src_c == src_cell_c then (dest_r,dest_c)
	else look_up tail (src_cell_r,src_cell_c)

--modify the image file, draw the path between 2 given cells
draw_segment (r1,c1) (r2,c2) maze_img =
	let upper_left = ((min c1 c2)*icon_l + intDiv (icon_l*3) 8, (min r1 r2)*icon_l + intDiv (icon_l*3) 8)
	in let lower_right = ((max c1 c2)*icon_l + intDiv (icon_l*5) 8, (max r1 r2)*icon_l + intDiv (icon_l*5) 8)
	in drawFilledRectangle upper_left lower_right 123 maze_img

--retrieve the path and draw it to image
trace_back path_lst h_cell src_cell x_cell maze_img = 
	do draw_segment cell src_cell maze_img
	   if h_cell == cell then return ()
	   else trace_back path_lst h_cell cell x_cell maze_img
	where cell = look_up path_lst src_cell

--check if exist the path from hero to exit door
exist_path x_cell [] = False
exist_path (x_cell_r,x_cell_c) ((src_r,src_c,_,_):tail) = if src_r == x_cell_r && src_c == x_cell_c then True 
							    else exist_path (x_cell_r,x_cell_c) tail

--run the BFS algorithm and draw the path
draw_path maze (h_cell_r,h_cell_c) x_cell maze_img = do path_lst <- bfs (Node h_cell_r h_cell_c) maze
							if (exist_path x_cell path_lst) == False then
								putStrLn "Cannot find the path"
							else
								trace_back path_lst (h_cell_r,h_cell_c) x_cell x_cell maze_img
							return ()
--read input file
parseMazeFromString file_str = do return $split "," $replace "\n" "" file_str

--main function
main = do h_icon <- loadPngFile "./images/hero-icon.png"
	  x_icon <- loadPngFile "./images/exit-icon.png"
	  file_str <- readFile "./test.txt"
	  maze <- parseMazeFromString file_str
	  maze_img <- newImage ((length (head maze))*icon_l,(length maze)*icon_l)
	  (h_cell, x_cell) <- draw_maze 0 0 maze_img maze (-1,-1) (-1,-1)
	  putStrLn "Begin finding path"
	  draw_path maze h_cell x_cell maze_img
	  putStrLn "End finding path"
	  copyRegion (0,0) (icon_l,icon_l) h_icon ((\(x,y) -> (icon_l*y, icon_l*x)) h_cell) maze_img
	  copyRegion (0,0) (icon_l,icon_l) x_icon ((\(x,y) -> (icon_l*y, icon_l*x)) x_cell) maze_img
	  savePngFile "./result.png" maze_img
