import f90nml
import os
import shutil

class WRFOfflineNesting:
    def __init__(self, master_namelist_path, output_dir):
        """
        Initialize the WRFOfflineNesting class.

        :param master_namelist_path: Path to the master multidomain namelist.input file.
        :param output_dir: Directory to save the modified namelists.
        """
        self.master_namelist_path = master_namelist_path
        self.output_dir = output_dir

        if not os.path.exists(self.output_dir):
            os.makedirs(self.output_dir)

    def parse_namelist(self):
        """Parse the master namelist file."""
        self.namelist = f90nml.read(self.master_namelist_path)

    def extract_domain_info(self):
        """Extract existing domain information from the namelist."""
        domain_info = []
        max_dom = self.namelist["domains"]["max_dom"]

        for domain_id in range(1, max_dom + 1):
            domain_data = {
                "domain_id": domain_id,
                "parent_id": self.namelist["domains"]["parent_id"][domain_id - 1],
                "parent_grid_ratio": self.namelist["domains"]["parent_grid_ratio"][domain_id - 1],
                "i_parent_start": self.namelist["domains"]["i_parent_start"][domain_id - 1],
                "j_parent_start": self.namelist["domains"]["j_parent_start"][domain_id - 1],
                "e_we": self.namelist["domains"]["e_we"][domain_id - 1],
                "e_sn": self.namelist["domains"]["e_sn"][domain_id - 1],
                "dx": self.namelist["domains"]["dx"],
                "dy": self.namelist["domains"]["dy"]
            }
            domain_info.append(domain_data)

        return domain_info

    def prepare_offline_nesting(self, nested_domain_id):
        """Prepare the namelist.input file for offline nesting with ndown.exe."""
        domain_info = self.extract_domain_info()

        if nested_domain_id <= 1 or nested_domain_id > len(domain_info):
            raise ValueError("Invalid nested domain ID. It must be greater than 1 and within the range of defined domains.")

        parent_domain = domain_info[nested_domain_id - 2]  # Parent domain (one before the nested domain)
        nested_domain = domain_info[nested_domain_id - 1]  # Nested domain

        # Extract relevant parameters
        i_parent_start = nested_domain["i_parent_start"]
        j_parent_start = nested_domain["j_parent_start"]
        e_we = nested_domain["e_we"]
        e_sn = nested_domain["e_sn"]

        dx = parent_domain["dx"]
        dy = parent_domain["dy"]

        # Update namelist for offline nesting
        self.namelist["domains"]["parent_id"] = [1]
        self.namelist["domains"]["parent_grid_ratio"] = [parent_domain["parent_grid_ratio"]]
        self.namelist["domains"]["i_parent_start"] = [i_parent_start]
        self.namelist["domains"]["j_parent_start"] = [j_parent_start]
        self.namelist["domains"]["e_we"] = [e_we]
        self.namelist["domains"]["e_sn"] = [e_sn]
        self.namelist["domains"]["max_dom"] = 2

        # Update dx and dy
        self.namelist["domains"]["dx"] = dx
        self.namelist["domains"]["dy"] = dy

    def save_modified_namelist(self):
        """Save the modified namelist to the output directory."""
        output_path = os.path.join(self.output_dir, "namelist.input")
        f90nml.write(self.namelist, output_path)
        print(f"Modified namelist for offline nesting saved to {output_path}")

    def rename_wrfinput(self, input_dir, nested_domain_id):
        """Rename wrfinput files for offline nesting preparation."""
        domain_info = self.extract_domain_info()

        if nested_domain_id <= 1 or nested_domain_id > len(domain_info):
            raise ValueError("Invalid nested domain ID. It must be greater than 1 and within the range of defined domains.")

        parent_domain_id = domain_info[nested_domain_id - 2]["domain_id"]
        nested_domain_id = domain_info[nested_domain_id - 1]["domain_id"]

        parent_wrfinput = os.path.join(input_dir, f"wrfinput_d{parent_domain_id:02d}")
        nested_wrfinput = os.path.join(input_dir, f"wrfinput_d{nested_domain_id:02d}")

        if os.path.exists(parent_wrfinput) and os.path.exists(nested_wrfinput):
            parent_wrfinput_out = os.path.join(self.output_dir, "wrfinput_d01")
            nested_wrfinput_out = os.path.join(self.output_dir, "wrfinput_d02")

            shutil.copy(parent_wrfinput, parent_wrfinput_out)
            shutil.copy(nested_wrfinput, nested_wrfinput_out)

            print(f"Parent wrfinput file renamed to {parent_wrfinput_out}")
            print(f"Nested wrfinput file renamed to {nested_wrfinput_out}")
        else:
            raise FileNotFoundError("Required wrfinput files for offline nesting not found.")

    def automate_offline_nesting(self, input_dir, nested_domain_id):
        """Run the complete offline nesting preparation process."""
        self.parse_namelist()
        print("Extracted Domain Information:", self.extract_domain_info())

        self.prepare_offline_nesting(nested_domain_id)
        self.save_modified_namelist()
        self.rename_wrfinput(input_dir, nested_domain_id)

# Example usage
if __name__ == "__main__":
    master_namelist_path = "path/to/master/namelist.input"
    output_dir = "path/to/output/directory"
    input_dir = "path/to/wrfinput/files"
    nested_domain_id = 3  # Specify the nested domain ID for offline nesting

    offline_nester = WRFOfflineNesting(master_namelist_path, output_dir)
    offline_nester.automate_offline_nesting(input_dir, nested_domain_id)

