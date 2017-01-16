faces = []
triangles__shared_coordinates = []
with open ("decimated_standford_bunny.ply", 'r') as f:
	lines = f.readlines()
	for line in lines:
		vertices = line.split()
		int_vertices = map(float, vertices)
		if int_vertices[0] == 3:
			faces.append((int_vertices[1],int_vertices[2],int_vertices[3]))
		else:
			triangle_coordinate = (int_vertices[0] *10,int_vertices[1] *10,int_vertices[2] *10)
			triangles__shared_coordinates.append(triangle_coordinate)
print faces[0], len(faces)
print triangles__shared_coordinates[0], len(triangles__shared_coordinates)

def prepareString(d):
	return str(d)[1:-1].replace(",","")
with open ("triangles.txt", 'w') as f:
	f.write('\n'.join(prepareString(d).replace(".0","") for d in faces))

with open ("vertices.txt", 'w') as f:
	f.write('\n'.join(prepareString(d) for d in triangles__shared_coordinates))
