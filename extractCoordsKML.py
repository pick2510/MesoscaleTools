from bs4 import BeautifulSoup
import sys


def main():
    if len(sys.argv) != 2:
        print("Coordinate Extractor from KML")
        print("usage: extractCoordsKML.py [filename]")
        sys.exit(-1)
    try:
        with open(sys.argv[1], "r") as f:
            soup = BeautifulSoup(f, "lxml")
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
    for coords in coords_string:
        coords_temp = coords[:-2]
        lon, lat = coords_temp.split(",")
        coords_dic["lon"].append(float(lon))
        coords_dic["lat"].append(float(lat))
   # print(min(coords_dic["lat"]), max(coords_dic["lat"), min(coords_dic["lon"]), max(coords_dic["lon"]))
    maxlat, minlat, maxlon, minlon = round(max(coords_dic["lat"]), 1), round(min(
        coords_dic["lat"]), 1), round(max(coords_dic["lon"]), 1),  round(min(coords_dic["lon"]), 1)
    gridstring = "{}/{}/{}/{}".format(maxlat,minlon,minlat,maxlon)
    print(gridstring)


if __name__ == "__main__":
    main()
