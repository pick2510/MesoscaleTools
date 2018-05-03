from bs4 import BeautifulSoup
import sys


def main():
    if len(sys.argv) != 2:
        print("Coordinate Extractor from KML")
        print("usage: extractCoordsKML.py [filename]")
        sys.exit(-1)
    try: 
        with open(sys.argv[1], "r") as f:
            soup = BeautifulSoup(f)
    except EnvironmentError:
        print("Error opening file {}".format(sys.argv[1]))
        sys.exit(-1)
    coords = soup.findAll("coordinates")
    if len(coords) > 1:
        raise ValueError("ERROR: Only one polygon supported")
    coords_string = coords[0].string.strip().split(" ")
    coords_dic = {}
    coords_dic["lat"] = []
    coords_dic["lon"] = []
    for i,coords in enumerate(coords_string):
        coords_temp = coords[:-2]
        print(coords_temp)
        splitted = coords_temp.split(",")
        for coord in splitted:
            print(coord)
            #coords_dic["lon"].append(coord[0][0])
            #coords_dic["lat"].append(coord[0][1])
    #print(coords_dic)


 










if __name__ == "__main__":
    main()