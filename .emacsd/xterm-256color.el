(setq tty-defined-color-alist
      '(("black" 0 0 0 0)
	("red" 1 52685 0 0)
	("green" 2 0 52685 0)
	("yellow" 3 52685 52685 0)
	("blue" 4 0 0 61166)
	("magenta" 5 52685 0 52685)
	("cyan" 6 0 52685 52685)
	("white" 7 58853 58853 58853)
	("brightblack" 8 32639 32639 32639)
	("brightred" 9 65535 0 0)
	("brightgreen" 10 0 65535 0)
	("brightyellow" 11 65535 65535 0)
	("brightblue" 12 23644 23644 65535)
	("brightmagenta" 13 65535 0 65535)
	("brightcyan" 14 0 65535 65535)
	("brightwhite" 15 65535 65535 65535)
	("color-16" 16 0 0 0)
	("color-17" 17 0 0 24415)
	("color-18" 18 0 0 34695)
	("color-19" 19 0 0 44975)
	("color-20" 20 0 0 55255)
	("color-21" 21 0 0 65535)
	("color-22" 22 0 24415 0)
	("color-23" 23 0 24415 24415)
	("color-24" 24 0 24415 34695)
	("color-25" 25 0 24415 44975)
	("color-26" 26 0 24415 55255)
	("color-27" 27 0 24415 65535)
	("color-28" 28 0 34695 0)
	("color-29" 29 0 34695 24415)
	("color-30" 30 0 34695 34695)
	("color-31" 31 0 34695 44975)
	("color-32" 32 0 34695 55255)
	("color-33" 33 0 34695 65535)
	("color-34" 34 0 44975 0)
	("color-35" 35 0 44975 24415)
	("color-36" 36 0 44975 34695)
	("color-37" 37 0 44975 44975)
	("color-38" 38 0 44975 55255)
	("color-39" 39 0 44975 65535)
	("color-40" 40 0 55255 0)
	("color-41" 41 0 55255 24415)
	("color-42" 42 0 55255 34695)
	("color-43" 43 0 55255 44975)
	("color-44" 44 0 55255 55255)
	("color-45" 45 0 55255 65535)
	("color-46" 46 0 65535 0)
	("color-47" 47 0 65535 24415)
	("color-48" 48 0 65535 34695)
	("color-49" 49 0 65535 44975)
	("color-50" 50 0 65535 55255)
	("color-51" 51 0 65535 65535)
	("color-52" 52 24415 0 0)
	("color-53" 53 24415 0 24415)
	("color-54" 54 24415 0 34695)
	("color-55" 55 24415 0 44975)
	("color-56" 56 24415 0 55255)
	("color-57" 57 24415 0 65535)
	("color-58" 58 24415 24415 0)
	("color-59" 59 24415 24415 24415)
	("color-60" 60 24415 24415 34695)
	("color-61" 61 24415 24415 44975)
	("color-62" 62 24415 24415 55255)
	("color-63" 63 24415 24415 65535)
	("color-64" 64 24415 34695 0)
	("color-65" 65 24415 34695 24415)
	("color-66" 66 24415 34695 34695)
	("color-67" 67 24415 34695 44975)
	("color-68" 68 24415 34695 55255)
	("color-69" 69 24415 34695 65535)
	("color-70" 70 24415 44975 0)
	("color-71" 71 24415 44975 24415)
	("color-72" 72 24415 44975 34695)
	("color-73" 73 24415 44975 44975)
	("color-74" 74 24415 44975 55255)
	("color-75" 75 24415 44975 65535)
	("color-76" 76 24415 55255 0)
	("color-77" 77 24415 55255 24415)
	("color-78" 78 24415 55255 34695)
	("color-79" 79 24415 55255 44975)
	("color-80" 80 24415 55255 55255)
	("color-81" 81 24415 55255 65535)
	("color-82" 82 24415 65535 0)
	("color-83" 83 24415 65535 24415)
	("color-84" 84 24415 65535 34695)
	("color-85" 85 24415 65535 44975)
	("color-86" 86 24415 65535 55255)
	("color-87" 87 24415 65535 65535)
	("color-88" 88 34695 0 0)
	("color-89" 89 34695 0 24415)
	("color-90" 90 34695 0 34695)
	("color-91" 91 34695 0 44975)
	("color-92" 92 34695 0 55255)
	("color-93" 93 34695 0 65535)
	("color-94" 94 34695 24415 0)
	("color-95" 95 34695 24415 24415)
	("color-96" 96 34695 24415 34695)
	("color-97" 97 34695 24415 44975)
	("color-98" 98 34695 24415 55255)
	("color-99" 99 34695 24415 65535)
	("color-100" 100 34695 34695 0)
	("color-101" 101 34695 34695 24415)
	("color-102" 102 34695 34695 34695)
	("color-103" 103 34695 34695 44975)
	("color-104" 104 34695 34695 55255)
	("color-105" 105 34695 34695 65535)
	("color-106" 106 34695 44975 0)
	("color-107" 107 34695 44975 24415)
	("color-108" 108 34695 44975 34695)
	("color-109" 109 34695 44975 44975)
	("color-110" 110 34695 44975 55255)
	("color-111" 111 34695 44975 65535)
	("color-112" 112 34695 55255 0)
	("color-113" 113 34695 55255 24415)
	("color-114" 114 34695 55255 34695)
	("color-115" 115 34695 55255 44975)
	("color-116" 116 34695 55255 55255)
	("color-117" 117 34695 55255 65535)
	("color-118" 118 34695 65535 0)
	("color-119" 119 34695 65535 24415)
	("color-120" 120 34695 65535 34695)
	("color-121" 121 34695 65535 44975)
	("color-122" 122 34695 65535 55255)
	("color-123" 123 34695 65535 65535)
	("color-124" 124 44975 0 0)
	("color-125" 125 44975 0 24415)
	("color-126" 126 44975 0 34695)
	("color-127" 127 44975 0 44975)
	("color-128" 128 44975 0 55255)
	("color-129" 129 44975 0 65535)
	("color-130" 130 44975 24415 0)
	("color-131" 131 44975 24415 24415)
	("color-132" 132 44975 24415 34695)
	("color-133" 133 44975 24415 44975)
	("color-134" 134 44975 24415 55255)
	("color-135" 135 44975 24415 65535)
	("color-136" 136 44975 34695 0)
	("color-137" 137 44975 34695 24415)
	("color-138" 138 44975 34695 34695)
	("color-139" 139 44975 34695 44975)
	("color-140" 140 44975 34695 55255)
	("color-141" 141 44975 34695 65535)
	("color-142" 142 44975 44975 0)
	("color-143" 143 44975 44975 24415)
	("color-144" 144 44975 44975 34695)
	("color-145" 145 44975 44975 44975)
	("color-146" 146 44975 44975 55255)
	("color-147" 147 44975 44975 65535)
	("color-148" 148 44975 55255 0)
	("color-149" 149 44975 55255 24415)
	("color-150" 150 44975 55255 34695)
	("color-151" 151 44975 55255 44975)
	("color-152" 152 44975 55255 55255)
	("color-153" 153 44975 55255 65535)
	("color-154" 154 44975 65535 0)
	("color-155" 155 44975 65535 24415)
	("color-156" 156 44975 65535 34695)
	("color-157" 157 44975 65535 44975)
	("color-158" 158 44975 65535 55255)
	("color-159" 159 44975 65535 65535)
	("color-160" 160 55255 0 0)
	("color-161" 161 55255 0 24415)
	("color-162" 162 55255 0 34695)
	("color-163" 163 55255 0 44975)
	("color-164" 164 55255 0 55255)
	("color-165" 165 55255 0 65535)
	("color-166" 166 55255 24415 0)
	("color-167" 167 55255 24415 24415)
	("color-168" 168 55255 24415 34695)
	("color-169" 169 55255 24415 44975)
	("color-170" 170 55255 24415 55255)
	("color-171" 171 55255 24415 65535)
	("color-172" 172 55255 34695 0)
	("color-173" 173 55255 34695 24415)
	("color-174" 174 55255 34695 34695)
	("color-175" 175 55255 34695 44975)
	("color-176" 176 55255 34695 55255)
	("color-177" 177 55255 34695 65535)
	("color-178" 178 55255 44975 0)
	("color-179" 179 55255 44975 24415)
	("color-180" 180 55255 44975 34695)
	("color-181" 181 55255 44975 44975)
	("color-182" 182 55255 44975 55255)
	("color-183" 183 55255 44975 65535)
	("color-184" 184 55255 55255 0)
	("color-185" 185 55255 55255 24415)
	("color-186" 186 55255 55255 34695)
	("color-187" 187 55255 55255 44975)
	("color-188" 188 55255 55255 55255)
	("color-189" 189 55255 55255 65535)
	("color-190" 190 55255 65535 0)
	("color-191" 191 55255 65535 24415)
	("color-192" 192 55255 65535 34695)
	("color-193" 193 55255 65535 44975)
	("color-194" 194 55255 65535 55255)
	("color-195" 195 55255 65535 65535)
	("color-196" 196 65535 0 0)
	("color-197" 197 65535 0 24415)
	("color-198" 198 65535 0 34695)
	("color-199" 199 65535 0 44975)
	("color-200" 200 65535 0 55255)
	("color-201" 201 65535 0 65535)
	("color-202" 202 65535 24415 0)
	("color-203" 203 65535 24415 24415)
	("color-204" 204 65535 24415 34695)
	("color-205" 205 65535 24415 44975)
	("color-206" 206 65535 24415 55255)
	("color-207" 207 65535 24415 65535)
	("color-208" 208 65535 34695 0)
	("color-209" 209 65535 34695 24415)
	("color-210" 210 65535 34695 34695)
	("color-211" 211 65535 34695 44975)
	("color-212" 212 65535 34695 55255)
	("color-213" 213 65535 34695 65535)
	("color-214" 214 65535 44975 0)
	("color-215" 215 65535 44975 24415)
	("color-216" 216 65535 44975 34695)
	("color-217" 217 65535 44975 44975)
	("color-218" 218 65535 44975 55255)
	("color-219" 219 65535 44975 65535)
	("color-220" 220 65535 55255 0)
	("color-221" 221 65535 55255 24415)
	("color-222" 222 65535 55255 34695)
	("color-223" 223 65535 55255 44975)
	("color-224" 224 65535 55255 55255)
	("color-225" 225 65535 55255 65535)
	("color-226" 226 65535 65535 0)
	("color-227" 227 65535 65535 24415)
	("color-228" 228 65535 65535 34695)
	("color-229" 229 65535 65535 44975)
	("color-230" 230 65535 65535 55255)
	("color-231" 231 65535 65535 65535)
	("color-232" 232 2056 2056 2056)
	("color-233" 233 4626 4626 4626)
	("color-234" 234 7196 7196 7196)
	("color-235" 235 9766 9766 9766)
	("color-236" 236 12336 12336 12336)
	("color-237" 237 14906 14906 14906)
	("color-238" 238 17476 17476 17476)
	("color-239" 239 20046 20046 20046)
	("color-240" 240 22616 22616 22616)
	("color-241" 241 25186 25186 25186)
	("color-242" 242 27756 27756 27756)
	("color-243" 243 30326 30326 30326)
	("color-244" 244 32896 32896 32896)
	("color-245" 245 35466 35466 35466)
	("color-246" 246 38036 38036 38036)
	("color-247" 247 40606 40606 40606)
	("color-248" 248 43176 43176 43176)
	("color-249" 249 45746 45746 45746)
	("color-250" 250 48316 48316 48316)
	("color-251" 251 50886 50886 50886)
	("color-252" 252 53456 53456 53456)
	("color-253" 253 56026 56026 56026)
	("color-254" 254 58596 58596 58596)
	("color-255" 255 61166 61166 61166)))

(and (fboundp 'clear-face-cache)
     (clear-face-cache))
