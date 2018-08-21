/*
 * game boy emulator in c89
 *
 * ![](https://i.imgur.com/oTs6J8j.png)
 * ![](https://i.imgur.com/RKCbznF.gif)
 *
 * at the moment it only runs tetris and the dmg bootrom and has no sound,
 * but I'm gonna start working towards making it run pokemon
 *
 * dependencies: SDL2
 *
 * should be compatible at least with x86/x86\_64 windows, linux, freebsd
 * with gcc, clang, msvc
 *
 * # compiling
 * just run ```./build``` . it's aware of ```CC```, ```CFLAGS```,
 * ```LDFLAGS``` in case you need to override anything
 *
 * if you compile with ```./build -DKT_DEBUG``` you will get a full trace
 * on stdout (SLOW!) which is handy for debugging
 *
 * # license
 * this is free and unencumbered software released into the public domain.
 * refer to the attached UNLICENSE or http://unlicense.org/
 */

#include <SDL2/SDL.h>

int sys_snprintf(char* buf, int buf_len, char* fmt, ...)
{
    int res;
    va_list va;

    va_start(va, fmt);
    res = (int)SDL_vsnprintf(buf, SDL_max(0, buf_len), fmt, va);
    va_end(va);

    return res;
}

#define log_line __FILE__, __LINE__

#define log_puts(x) \
    log_print(log_line, "%s", x)

#define log_dump(spec, var) \
    log_print(log_line, #var " = %" spec, var)

void log_print(char const* file, int line, char* fmt, ...)
{
    va_list va;
    char* msg;
    int msg_len;
    char* p;
    char* end;

    msg_len = 0;
    msg_len += sys_snprintf(0, 0, "[%s:%d] ", file, line);
    va_start(va, fmt);
    msg_len += SDL_vsnprintf(0, 0, fmt, va);
    va_end(va);
    msg_len += 2;

    msg = SDL_malloc(msg_len);
    if (!msg) {
        SDL_Log("log_print alloc failed: %s", SDL_GetError());
        return;
    }

    p = msg;
    end = msg + msg_len;
    p += sys_snprintf(p, end - p, "[%s:%d] ", file, line);

    va_start(va, fmt);
    p += SDL_vsnprintf(p, end - p, fmt, va);
    va_end(va);

    for (p = msg; p < end; p += SDL_MAX_LOG_MESSAGE - 7) {
        SDL_Log("%s", p);
    }

    SDL_free(msg);
}

/* --------------------------------------------------------------------- */

#define stringify_(x) #x
#define stringify(x) stringify_(x)

char* sys_read_entire_file(char* path, int* len)
{
    SDL_RWops* io;
    char* res;
    int res_len;
    char buf[1024];
    int n;

    res = 0;
    res_len = 0;

    io = SDL_RWFromFile(path, "rb");
    if (!io) {
        log_puts(SDL_GetError());
        return 0;
    }

    while (1)
    {
        char* grown;

        n = (int)SDL_RWread(io, buf, 1, sizeof(buf));

        if (!n)
        {
            if (*SDL_GetError())
            {
                log_print(log_line, "SDL_RWread failed: %s",
                    SDL_GetError());
                goto cleanup;
            }

            break;
        }

        grown = SDL_realloc(res, res_len + n);

        if (!grown)
        {
            free(res);
            res = 0;
            log_print(log_line, "SDL_realloc failed: %s", SDL_GetError());
            goto cleanup;
        }

        res = grown;
        SDL_memcpy(res + res_len, buf, n);
        res_len += n;
    }

cleanup:
    SDL_RWclose(io);

    if (len) {
        *len = res_len;
    }

    return res;
}

/* --------------------------------------------------------------------- */

struct kt_cartidge
{
    char logo[0x48];
    char title[17];
    char manufacturer[5];
    int flags;
    char licensee[3];
    int type;
    int rom_len;
    int ram_len;
    int licensee_code;
    int version;
    int checksum;
    int global_checksum;
};

typedef struct kt_cartidge kt_cartidge_t;

#define KT_GBC (1<<1)
#define KT_GBC_ONLY (1<<2)
#define KT_SGB (1<<3)
#define KT_JAPANESE (1<<4)

#define KT_CARTIDGE_TYPES(wrap)\
    wrap(KT_ROM_ONLY               , 0x00) \
    wrap(KT_MBC1                   , 0x01) \
    wrap(KT_MBC1_RAM               , 0x02) \
    wrap(KT_MBC1_RAM_BATTERY       , 0x03) \
    wrap(KT_MBC2                   , 0x05) \
    wrap(KT_MBC2_BATTERY           , 0x06) \
    wrap(KT_ROM_RAM                , 0x08) \
    wrap(KT_ROM_RAM_BATTERY        , 0x09) \
    wrap(KT_MMM01                  , 0x0B) \
    wrap(KT_MMM01_RAM              , 0x0C) \
    wrap(KT_MMM01_RAM_BATTERY      , 0x0D) \
    wrap(KT_MBC3_TIMER_BATTERY     , 0x0F) \
    wrap(KT_MBC3_TIMER_RAM_BATTERY , 0x10) \
    wrap(KT_MBC3                   , 0x11) \
    wrap(KT_MBC3_RAM               , 0x12) \
    wrap(KT_MBC3_RAM_BATTERY       , 0x13) \
    wrap(KT_MBC4                   , 0x15) \
    wrap(KT_MBC4_RAM               , 0x16) \
    wrap(KT_MBC4_RAM_BATTERY       , 0x17) \
    wrap(KT_MBC5                   , 0x19) \
    wrap(KT_MBC5_RAM               , 0x1A) \
    wrap(KT_MBC5_RAM_BATTERY       , 0x1B) \
    wrap(KT_MBC5_RUMBLE            , 0x1C) \
    wrap(KT_MBC5_RUMBLE_RAM        , 0x1D) \
    wrap(KT_MBC5_RUMBLE_RAM_BATTERY, 0x1E) \
    wrap(KT_POCKET_CAMERA          , 0xFC) \
    wrap(KT_BANDAI_TAMA5           , 0xFD) \
    wrap(KT_HUC3                   , 0xFE) \
    wrap(KT_HUC1_RAM_BATTERY       , 0xFF) \

/* good ol' macro vodoo */

#define w(x, y) x = y,
enum kt_cartidge_type {
    KT_CARTIDGE_TYPES(w)
    KT_LAST_CARTIDGE_TYPE
};
#undef w

#define w(x, y) case x: return stringify(x);
char* kt_cartidge_type_str(int type)
{
    switch (type)
    {
        KT_CARTIDGE_TYPES(w)
    }

    return "unknown";
}
#undef w

void kt_print_cart(kt_cartidge_t* cart)
{
    log_dump("s", cart->title);
    log_dump("d", cart->licensee_code);
    log_dump("d", cart->rom_len);
    log_dump("d", cart->ram_len);
    log_dump("d", cart->version);
    log_dump("02X", cart->checksum);
    log_dump("04X", cart->global_checksum);
    log_print(log_line, "cart->type = [%02X] %s", cart->type,
        kt_cartidge_type_str(cart->type));

    if (strlen(cart->manufacturer)) {
        log_dump("s", cart->manufacturer);
    }

    if (cart->flags & KT_GBC)
    {
        if (cart->flags & KT_GBC_ONLY) {
            log_puts("gbc-only");
        } else {
            log_puts("supports gbc");
        }
    }

    if (cart->flags & KT_SGB) {
        log_puts("supports sgb");
    }

    if (cart->flags & KT_JAPANESE) {
        log_puts("japanese game");
    }

    if (cart->licensee_code == 0x33) {
        log_dump("s", cart->licensee);
    }
}

int kt_parse_cart(kt_cartidge_t* cart, Uint8* data, int data_len)
{
    int title_len;

    if (data_len < 0x150) {
        log_puts("must be at least 0x150 bytes");
        return 0;
    }

    SDL_memset(cart, 0, sizeof(kt_cartidge_t));
    SDL_memcpy(cart->logo, &data[0x104], 0x48);

    title_len = 16;

    if (data[0x143] & 0x80)
    {
        cart->flags |= KT_GBC;

        if (data[0x143] & 0x40) {
            cart->flags |= KT_GBC_ONLY;
        }

        title_len = 15;

        /* early gbc games might not have the manufacturer code */
        if (data[0x13F] && !data[0x13E]) {
            title_len = 11;
            SDL_memcpy(cart->manufacturer, &data[0x13F], 4);
        }
    }

    SDL_memcpy(cart->title, &data[0x134], title_len);
    cart->licensee_code = data[0x14B];

    if (cart->licensee_code == 0x33) {
        SDL_memcpy(cart->licensee, &data[0x144], 2);
    }

    if (data[0x146] == 0x03) {
        cart->flags |= KT_SGB;
    }

    cart->type = data[0x147];

    if (data[0x148] <= 0x07) {
        cart->rom_len = 32000 << data[0x148];
    } else {
        switch (data[0x148])
        {
        case 0x52: cart->rom_len = 1100000; break;
        case 0x53: cart->rom_len = 1200000; break;
        case 0x54: cart->rom_len = 1500000; break;
        default:
            log_print(log_line, "unknown rom len %02X", data[0x148]);
            return 0;
        }
    }

    switch (data[0x149])
    {
    case 0x00: cart->ram_len = 0; break;
    case 0x01: cart->ram_len = 2000; break;
    case 0x02: cart->ram_len = 8000; break;
    case 0x03: cart->ram_len = 32000; break;
    default:
        log_print(log_line, "unknown ram len %02X", data[0x149]);
        return 0;
    }

    if (!data[0x14A]) {
        cart->flags |= KT_JAPANESE;
    }

    cart->version = data[0x14C];
    cart->checksum = data[0x14D];
    cart->global_checksum = (data[0x14E] << 8) | data[0x14F];

    return 1;
}

Uint8 kt_cart_hdr_checksum(Uint8* data)
{
    int i;
    int res;

    res = 0;

    for (i = 0x0134; i <= 0x014C; ++i) {
        res -= data[i] + 1;
    }

    return res & 0xFF;
}

/* --------------------------------------------------------------------- */

enum kouta_mode
{
    KT_DMG_MODE,
    KT_GBC_MODE,
    KT_LAST_MODE
};

#define KT_MEM_ROM_ONLY 1
#define KT_MEM_MBC1 2
#define KT_MEM_MBC3 5
#define KT_MEM_HAS_RAM 0x80000000

#define KT_LO 0x01
#define KT_HI 0x02

#define KT_AF 0x00
#define KT_BC 0x10
#define KT_DE 0x20
#define KT_HL 0x30
#define KT_SP 0x40

#define KT_A (KT_AF|KT_HI)
#define KT_F (KT_AF|KT_LO)
#define KT_B (KT_BC|KT_HI)
#define KT_C (KT_BC|KT_LO)
#define KT_D (KT_DE|KT_HI)
#define KT_E (KT_DE|KT_LO)
#define KT_H (KT_HL|KT_HI)
#define KT_L (KT_HL|KT_LO)

#define KT_WIDTH 160
#define KT_HEIGHT 144
#define KT_DMG_FREQUENCY 4194304

#define KT_TAC_START 0x04
#define KT_TAC_CLOCK_BITS 0x03

int kt_tima_clocks[] = { 1024, 16, 64, 256 };

#define KT_JOYP_A (1<<0)
#define KT_JOYP_B (1<<1)
#define KT_JOYP_SELECT (1<<2)
#define KT_JOYP_START (1<<3)

#define KT_JOYP_RIGHT (1<<0)
#define KT_JOYP_LEFT (1<<1)
#define KT_JOYP_UP (1<<2)
#define KT_JOYP_DOWN (1<<3)

#define KT_JOYP_DPAD (1<<4)
#define KT_JOYP_BUTTONS (1<<5)

#define KT_ZERO 0x0080
#define KT_SUBTRACT 0x0040
#define KT_HCARRY 0x0020
#define KT_CARRY 0x0010

#define KT_IME_DI (1<<0)
#define KT_IME_EI (1<<1)

#define KT_NR50_VIN_SO2 0x80
#define KT_NR50_SO2_LEVEL_BITS 0x70
#define KT_NR50_VIN_SO1 0x08
#define KT_NR50_SO1_LEVEL_BITS 0x07

#define KT_NR51_SOUND4_S02 0x80
#define KT_NR51_SOUND3_S02 0x40
#define KT_NR51_SOUND2_S02 0x20
#define KT_NR51_SOUND1_S02 0x10
#define KT_NR51_SOUND4_S01 0x08
#define KT_NR51_SOUND3_S01 0x04
#define KT_NR51_SOUND2_S01 0x02
#define KT_NR51_SOUND1_S01 0x01

#define KT_NR52_ENABLE 0x80
#define KT_NR52_SOUND4 0x08
#define KT_NR52_SOUND3 0x04
#define KT_NR52_SOUND2 0x02
#define KT_NR52_SOUND1 0x01

#define KT_LCDC_ENABLE 0x80
#define KT_LCDC_WINDOW_MAP 0x40
#define KT_LCDC_WINDOW_ENABLE 0x20
#define KT_LCDC_TILES 0x10
#define KT_LCDC_BG_MAP 0x08
#define KT_LCDC_OBJ_8x16 0x04
#define KT_LCDC_OBJ_ENABLE 0x02
#define KT_LCDC_BG_ENABLE 0x01

#define KT_STAT_INT_COINCIDENCE 0x40
#define KT_STAT_INT_OAM 0x20
#define KT_STAT_INT_VBLANK 0x10
#define KT_STAT_INT_HBLANK 0x08
#define KT_STAT_COINCIDENCE 0x04
#define KT_STAT_MODE_BITS 0x03
#define KT_STAT_READING_OAM_VRAM 0x03
#define KT_STAT_READING_OAM 0x02
#define KT_STAT_VBLANK 0x01
#define KT_STAT_HBLANK 0x00

#define KT_OBJ_BEHIND 0x80
#define KT_OBJ_FLIP_Y 0x40
#define KT_OBJ_FLIP_X 0x20
#define KT_OBJ_PALETTE 0x10
#define KT_OBJ_GBC_BANK 0x08
#define KT_OBJ_GBC_PALETTE_BITS 0x07

#define KT_INT_VBLANK 0x01
#define KT_INT_STAT 0x02
#define KT_INT_TIMER 0x04
#define KT_INT_SERIAL 0x08
#define KT_INT_JOYPAD 0x10

char kt_dmg_rom[] =
{
    0x31, 0xFE, 0xFF, 0xAF, 0x21, 0xFF, 0x9F, 0x32, 0xCB, 0x7C, 0x20, 0xFB,
    0x21, 0x26, 0xFF, 0x0E, 0x11, 0x3E, 0x80, 0x32, 0xE2, 0x0C, 0x3E, 0xF3,
    0xE2, 0x32, 0x3E, 0x77, 0x77, 0x3E, 0xFC, 0xE0, 0x47, 0x11, 0x04, 0x01,
    0x21, 0x10, 0x80, 0x1A, 0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
    0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06, 0x08, 0x1A, 0x13, 0x22,
    0x23, 0x05, 0x20, 0xF9, 0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99,
    0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20, 0xF9, 0x2E, 0x0F, 0x18,
    0xF3, 0x67, 0x3E, 0x64, 0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,
    0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90, 0x20, 0xFA, 0x0D, 0x20,
    0xF7, 0x1D, 0x20, 0xF2, 0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62,
    0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06, 0x7B, 0xE2, 0x0C, 0x3E,
    0x87, 0xE2, 0xF0, 0x42, 0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,
    0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04, 0xC5, 0xCB, 0x11, 0x17,
    0xC1, 0xCB, 0x11, 0x17, 0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9,

    0xCE, 0xED, 0x55, 0x45, 0x88, 0x09, 0x00, 0x0E, 0x00, 0x07, 0x00, 0x03,
    0x00, 0x0B, 0x00, 0x03, 0x00, 0x09, 0x00, 0x0D, 0x00, 0x09, 0x00, 0x0C,
    0xDC, 0xCC, 0x5D, 0xD5, 0xBB, 0xB9, 0x00, 0x0E, 0xCF, 0xC7, 0xBB, 0x3B,
    0x51, 0x11, 0x67, 0x63, 0x5D, 0x1D, 0xA8, 0x88, 0xBB, 0xB9, 0x2E, 0x0E,

    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

    0x21, 0x04, 0x01, 0x11,
    0xA8, 0x00, 0x1A, 0x13, 0xBE, 0x20, 0xFE, 0x23, 0x7D, 0xFE, 0x34, 0x20,
    0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xFB, 0x86, 0x20, 0xFE,
    0x3E, 0x01, 0xE0, 0x50
};

/* --------------------------------------------------------------------- */

struct kouta
{
    int mode;
    int cycles_per_second;
    Uint64 n_cycles;
    int entered_vblank;
    int halt;
    int stop;
    int error;
    int disable_dmg_rom;
    Uint8 dpad;
    Uint8 buttons;

    Uint16 regs[5];
    Uint16 pc;

    int ime_events;
    int ime;

    Uint8* cart;
    int cart_len;
    int memory_model;
    int rom_bank;
    int ram_bank;
    int ram_enabled;
    int bank_mode;

    Uint8 vram[0x2000];
    Uint8 wram[0x2000];
    Uint8 hram[0x7F];
    Uint8 ram[0x8000];

    int div_cycles;
    int tima_cycles;
    Uint8 div;
    Uint8 tima;
    Uint8 tma;
    Uint8 tac;

    Uint8 joyp;

    Uint8 nr50;
    Uint8 nr51;
    Uint8 nr52;

    int n_lcd_cycles;
    Uint8 lcdc;
    Uint8 stat;
    Uint8 scy;
    Uint8 scx;
    Uint8 ly;
    Uint8 lyc;
    Uint8 wy;
    Uint8 wx;
    int dma_cycles;
    Uint8 bgp;
    Uint8 obp[2];

    Uint8 ie;
    Uint8 if_;

    Uint8 oam[0xA0];

#ifdef KT_LOG_SERIAL
    Uint8 serial_out[4096];
    int serial_out_len;
#endif
};

typedef struct kouta kouta_t;

void kt_print_regs(kouta_t* kt)
{
    Uint16 af;

    af = kt->regs[KT_AF >> 4];

    SDL_Log("AF %04X [%s %s %s %s] | BC %04X | DE %04X | HL %04X | SP %04X",
        af,
        af & KT_ZERO ? " Z" : "NZ",
        af & KT_SUBTRACT ? "N" : " ",
        af & KT_HCARRY ? "H" : " ",
        af & KT_CARRY ? " C" : "NC",
        kt->regs[KT_BC >> 4], kt->regs[KT_DE >> 4],
        kt->regs[KT_HL >> 4], kt->regs[KT_SP >> 4]
    );
}

Uint16 kt_get_reg(kouta_t* kt, int reg)
{
    Uint16 full;

    full = kt->regs[reg >> 4];

    if (reg & KT_LO) {
        return (Uint16)(full & 0x00FF);
    } else if (reg & KT_HI) {
        return (Uint16)((full & 0xFF00) >> 8);
    }

    return full;
}

void kt_set_a(kouta_t* kt, Uint8 value)
{
    kt->regs[KT_AF >> 4] &= 0x00FF;
    kt->regs[KT_AF >> 4] |= (value << 8) & 0xFF00;
}

void kt_set_reg(kouta_t* kt, int reg, Uint16 value)
{
    int mask;
    int index;

    index = reg >> 4;
    mask = 0;

    if (reg & KT_LO) {
        mask = 0xFF00;
    } else if (reg & KT_HI) {
        mask = 0x00FF;
        value <<= 8;
    }

    kt->regs[index] &= mask;
    kt->regs[index] |= (value & (~mask));
}

void kt_set_flag(kouta_t* kt, int flag, int set)
{
    if (set) {
        kt->regs[KT_AF >> 4] |= flag;
    } else {
        kt->regs[KT_AF >> 4] &= ~flag;
    }
}

void kt_reset(kouta_t* kt)
{
    memset(kt, 0, sizeof(kouta_t));
    kt->cycles_per_second = KT_DMG_FREQUENCY;
    kt->stat = KT_STAT_READING_OAM;
    kt->ime = 1;
    kt->rom_bank = 1;
}

void kt_write(kouta_t* kt, int addr, Uint8 value);

void kt_skip_bootrom(kouta_t* kt)
{
    kt->pc = 0x100;
    kt->regs[KT_AF >> 4] = 0x01B0;
    kt->regs[KT_BC >> 4] = 0x0013;
    kt->regs[KT_DE >> 4] = 0x00D8;
    kt->regs[KT_HL >> 4] = 0x014D;
    kt->regs[KT_SP >> 4] = 0xFFFE;
    kt_write(kt, 0xFF05, 0x00); /* TIMA */
    kt_write(kt, 0xFF06, 0x00); /* TMA */
    kt_write(kt, 0xFF07, 0x00); /* TAC */
    kt_write(kt, 0xFF10, 0x80); /* NR10 */
    kt_write(kt, 0xFF11, 0xBF); /* NR11 */
    kt_write(kt, 0xFF12, 0xF3); /* NR12 */
    kt_write(kt, 0xFF14, 0xBF); /* NR14 */
    kt_write(kt, 0xFF16, 0x3F); /* NR21 */
    kt_write(kt, 0xFF17, 0x00); /* NR22 */
    kt_write(kt, 0xFF19, 0xBF); /* NR24 */
    kt_write(kt, 0xFF1A, 0x7F); /* NR30 */
    kt_write(kt, 0xFF1B, 0xFF); /* NR31 */
    kt_write(kt, 0xFF1C, 0x9F); /* NR32 */
    kt_write(kt, 0xFF1E, 0xBF); /* NR33 */
    kt_write(kt, 0xFF20, 0xFF); /* NR41 */
    kt_write(kt, 0xFF21, 0x00); /* NR42 */
    kt_write(kt, 0xFF22, 0x00); /* NR43 */
    kt_write(kt, 0xFF23, 0xBF); /* NR30 */
    kt_write(kt, 0xFF24, 0x77); /* NR50 */
    kt_write(kt, 0xFF25, 0xF3); /* NR51 */
    kt_write(kt, 0xFF26, 0xF1); /* NR52 (F0 for SGB) */
    kt_write(kt, 0xFF40, 0x91); /* LCDC */
    kt_write(kt, 0xFF42, 0x00); /* SCY */
    kt_write(kt, 0xFF43, 0x00); /* SCX */
    kt_write(kt, 0xFF45, 0x00); /* LYC */
    kt_write(kt, 0xFF47, 0xFC); /* BGP */
    kt_write(kt, 0xFF48, 0xFF); /* OBP0 */
    kt_write(kt, 0xFF49, 0xFF); /* OBP1 */
    kt_write(kt, 0xFF4A, 0x00); /* WY */
    kt_write(kt, 0xFF4B, 0x00); /* WX */
    kt_write(kt, 0xFF50, 0x01); /* disable DMG rom */
    kt_write(kt, 0xFFFF, 0x00); /* IE */

    log_puts("*** SKIPPED BOOTROM ***");
}

int kt_load_rom(kouta_t* kt, char *path)
{
    kt_cartidge_t cart_info;
    Uint8 checksum;

    kt->cart = (Uint8*)sys_read_entire_file(path, &kt->cart_len);

    if (!kt->cart ||
        !kt_parse_cart(&cart_info, kt->cart, kt->cart_len))
    {
        return 0;
    }

    kt_print_cart(&cart_info);

    if (cart_info.flags & KT_GBC_ONLY) {
        log_puts("gbc mode not yet implemented");
        return 0;
    }

    checksum = kt_cart_hdr_checksum(kt->cart);

    if (checksum != cart_info.checksum) {
        log_puts("header checksum doesn't match");
        log_dump("02X", checksum);
        return 0;
    }

    switch (cart_info.type)
    {
    case KT_ROM_ONLY:
        kt->memory_model = KT_MEM_ROM_ONLY;
        break;
    case KT_MBC1:
    case KT_MBC1_RAM:
    case KT_MBC1_RAM_BATTERY:
        kt->memory_model = KT_MEM_MBC3;
        break;
    case KT_MBC3_TIMER_BATTERY:
    case KT_MBC3_TIMER_RAM_BATTERY:
    case KT_MBC3:
    case KT_MBC3_RAM:
    case KT_MBC3_RAM_BATTERY:
        kt->memory_model = KT_MEM_MBC3;
        break;
    default:
        log_puts("unimplemented memory model");
        return 0;
    }

    switch (cart_info.type)
    {
    case KT_MBC1_RAM:
    case KT_MBC3_TIMER_RAM_BATTERY:
    case KT_MBC3_RAM:
    case KT_MBC3_RAM_BATTERY:
        kt->memory_model |= KT_MEM_HAS_RAM;
        break;
    }

    /* bypass checksum */
    memcpy(&kt->cart[0x104], &kt_dmg_rom[168], 0x48);
    kt->cart[0x14D] = kt_cart_hdr_checksum(kt->cart);

    return 1;
}

Uint8 kt_read(kouta_t* kt, int addr)
{
    addr &= 0xFFFF;

#if 0
    /*
     * removed for now because I don't know if ret and stack operations
     * are supposed to bypass this or not, and at the moment it's not
     * precise enough so i hit ret's while still in dma and it fucks the
     * return address
     */

    if (kt->dma_cycles > 0 && (addr < 0xFF80 || addr > 0xFFFE)) {
        log_puts("can only access hram during oam dma");
        log_dump("d", kt->dma_cycles);
    }

    else
#endif
    if (!kt->disable_dmg_rom && addr < 0x100) {
        return kt_dmg_rom[addr];
    }

    else if (addr < 0x4000) {
        return kt->cart[addr];
    }

    else if (addr < 0x8000) {
        return kt->cart[kt->rom_bank * 0x4000 + (addr & 0x3FFF)];
    }

    else if (addr < 0xA000) {
        return kt->vram[addr & 0x1FFF];
    }

    else if (addr < 0xC000)
    {
        if (kt->ram_bank < 4)
        {
            if (!kt->ram_enabled) {
                log_puts("tried to write disabled ram");
            }

            else if (!(kt->memory_model & KT_MEM_HAS_RAM)) {
                log_puts("tried to write external ram but has none");
            }

            else {
                return kt->ram[kt->ram_bank * 0x2000 + (addr & 0x1FFF)];
            }
        }

        else if (kt->ram_bank >= 0x08 || kt->ram_bank <= 0x0C) {
            /* TODO: rtc */
            return 0;
        }

        else {
            log_puts("tried to read with invalid ram bank selected");
        }
    }

    else if (addr < 0xFE00) {
        return kt->wram[addr & 0x1FFF];
    }

    else if (addr < 0xFEA0) {
        return kt->oam[addr & 0x00FF];
    }

    else if (addr < 0xFF00) {
        log_puts("tried to read unusable area");
    }

    else if (addr < 0xFF80) {
        switch (addr)
        {
        case 0xFF01: return 0; /* TODO: serial cable shit */
        case 0xFF02: return 0;
        case 0xFF00: return kt->joyp;
        case 0xFF04: return kt->div;
        case 0xFF05: return kt->tima;
        case 0xFF06: return kt->tma;
        case 0xFF07: return kt->tac;
        case 0xFF0F: return kt->if_;
        case 0xFF24: return kt->nr50;
        case 0xFF25: return kt->nr51;
        case 0xFF26: return kt->nr52;
        case 0xFF40: return kt->lcdc;
        case 0xFF41: return kt->stat;
        case 0xFF42: return kt->scy;
        case 0xFF43: return kt->scx;
        case 0xFF44: return kt->ly;
        case 0xFF45: return kt->lyc;
        case 0xFF47: return kt->bgp;
        case 0xFF48: return kt->obp[0];
        case 0xFF49: return kt->obp[1];
        case 0xFF4A: return kt->wy;
        case 0xFF4B: return kt->wx;

        default:
            log_puts("tried to read unimplemented or invalid I/O port");
        }
    }

    else if (addr < 0xFFFF) {
        return kt->hram[addr & 0x007F];
    }

    else {
        return kt->ie;
    }

    log_dump("04X", addr);

    return 0xFF;
}

void kt_dma(kouta_t* kt, int start_addr)
{
    int i;

#ifdef KT_DEBUG
    log_print(log_line, "*** DMA %04X ***", start_addr);
#endif

    for (i = 0; i < 0xA0; ++i) {
        kt->oam[i] = kt_read(kt, start_addr + i);
    }

    kt->dma_cycles = 671;
}

void kt_update_joyp(kouta_t* kt)
{
    Uint8 buttons;

    /*
     * the logic for this is very confusing because every bit is flipped
     * so bits are 0 when the button is pressed, and the buttons/dpad
     * selection is also like that
     */

    kt->joyp &= (KT_JOYP_BUTTONS | KT_JOYP_DPAD);

    if (!(kt->joyp & KT_JOYP_BUTTONS)) {
        buttons = kt->buttons;
    } else if (!(kt->joyp & KT_JOYP_DPAD)) {
        buttons = kt->dpad;
    } else {
        buttons = 0;
    }

    buttons = ~buttons;
    buttons &= ~(KT_JOYP_BUTTONS | KT_JOYP_DPAD);
    kt->joyp |= buttons;
}

void kt_write(kouta_t* kt, int addr, Uint8 value)
{
    addr &= 0xFFFF;

    if (addr < 0x8000)
    {
        int model;

        model = kt->memory_model & 0x0F;

        switch (model)
        {
        case KT_MEM_MBC1:
        case KT_MEM_MBC3:
            if (addr < 0x2000)
            {
                if ((value & 0x0F) == 0x0A) {
                    kt->ram_enabled = 1;
                } else if (value == 0x00) {
                    kt->ram_enabled = 0;
                }
            }

            else if (addr < 0x4000)
            {
                int low;

                low = SDL_max(1, value);

                if (model == KT_MEM_MBC1) {
                    kt->rom_bank &= ~0x1F;
                    low &= 0x1F;
                } else {
                    kt->rom_bank &= ~0x7F;
                    low &= 0x7F;
                }

                kt->rom_bank |= low;
            }

            else if (addr < 0x6000)
            {
                int high;

                high = value & 3;

                if (kt->bank_mode) {
                    kt->ram_bank = high;
                } else {
                    kt->rom_bank &= 0x1F;
                    kt->rom_bank |= high;
                }
            }

            else if (model == KT_MEM_MBC1) {
                kt->bank_mode = value & 1;
            } else {
                /* TODO: latch clock */
            }
            return;

        default:
            log_puts("tried to write to rom");
        }
    }

    else if (addr < 0xA000) {
        kt->vram[addr & 0x1FFF] = value;
        return;
    }

    else if (addr < 0xC000)
    {
        if (kt->ram_bank < 4) {
            kt->ram[kt->ram_bank * 0x2000 + (addr & 0x1FFF)] = value;
            return;
        }

        if (kt->ram_bank >= 0x08 || kt->ram_bank <= 0x0C) {
            /* TODO: rtc */
            return;
        }

        log_puts("tried to write with invalid ram bank selected");
    }

    else if (addr < 0xFE00) {
        kt->wram[addr & 0x1FFF] = value;
        return;
    }

    else if (addr < 0xFEA0) {
        kt->oam[addr & 0x00FF] = value;
        return;
    }

    else if (addr < 0xFF00) {
        log_puts("tried to write to unusable area");
    }

    else if (addr < 0xFF80) {
        switch (addr)
        {
        case 0xFF00:
            value &= KT_JOYP_BUTTONS | KT_JOYP_DPAD;
            kt->joyp &= ~(KT_JOYP_BUTTONS | KT_JOYP_DPAD);
            kt->joyp |= value;
            kt_update_joyp(kt);
            return;
        case 0xFF01:
#ifdef KT_LOG_SERIAL
            if (value == 0x0A ||
                kt->serial_out_len >= (int)sizeof(kt->serial_out) - 1)
            {
                kt->serial_out[kt->serial_out_len] = 0;
                SDL_Log("%s", kt->serial_out);
                kt->serial_out_len = 0;
            }

            kt->serial_out[kt->serial_out_len++] = value;
#endif
            return;
        case 0xFF02: return;
        case 0xFF04: kt->div = 0; return;
        case 0xFF05: kt->tima = value; return;
        case 0xFF06: kt->tma = value; return;
        case 0xFF07: kt->tac = value; return;
        case 0xFF0F: kt->if_ = value; return;
        case 0xFF24: kt->nr50 = value; return;
        case 0xFF25: kt->nr51 = value; return;
        case 0xFF26:
            value &= KT_NR52_ENABLE;
            kt->nr52 &= ~KT_NR52_ENABLE;
            kt->nr52 |= value;
            return;
        case 0xFF40: kt->lcdc = value; return;
        case 0xFF41:
            value &= ~(KT_STAT_MODE_BITS | KT_STAT_COINCIDENCE);
            kt->stat &= KT_STAT_MODE_BITS | KT_STAT_COINCIDENCE;
            kt->stat |= value;
            return;
        case 0xFF42: kt->scy = value; return;
        case 0xFF43: kt->scx = value; return;
        case 0xFF44: kt->ly = 0; return;
        case 0xFF45: kt->lyc = value; return;
        case 0xFF46: kt_dma(kt, (value << 8) & 0xFF00); return;
        case 0xFF47: kt->bgp = value; return;
        case 0xFF48: kt->obp[0] = value; return;
        case 0xFF49: kt->obp[1] = value; return;
        case 0xFF4A: kt->wy = value; return;
        case 0xFF4B: kt->wx = value; return;
        case 0xFF50: kt->disable_dmg_rom = value; return;

        default:
            log_puts("tried to write unimplemented or invalid i/o port");
        }
    }

    else if (addr < 0xFFFF) {
        kt->hram[addr & 0x007F] = value;
        return;
    }

    else {
        kt->ie = value;
        return;
    }

    log_dump("04X", addr);
    log_dump("02X", value);
}

Uint16 kt_read2(kouta_t* kt, int addr)
{
    return (Uint16)(
        ((kt_read(kt, addr + 0) << 0) & 0x00FF) |
        ((kt_read(kt, addr + 1) << 8) & 0xFF00)
    );
}

void kt_write2(kouta_t* kt, int addr, Uint16 value)
{
    kt_write(kt, addr + 0, (value & 0x00FF) >> 0);
    kt_write(kt, addr + 1, (value & 0xFF00) >> 8);
}

void kt_push2(kouta_t* kt, Uint16 value)
{
    kt->regs[KT_SP >> 4] -= 2;
    kt_write2(kt, kt->regs[KT_SP >> 4], value);
}

Uint16 kt_pop2(kouta_t* kt)
{
    kt->regs[KT_SP >> 4] += 2;
    return kt_read2(kt, kt->regs[KT_SP >> 4] - 2);
}

/* --------------------------------------------------------------------- */

struct kt_op
{
    int op;
    char* name;
    int addr_dst, dst;
    int addr_src, src;
    void (* execute)(kouta_t* kt, struct kt_op* instruction);
    int size;
    int n_cycles;
};

typedef struct kt_op kt_op_t;

void kt_unimplemented(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    log_puts("unimplemented instruction");
    kt->error = 1;
}

void kt_nop(kouta_t* kt, kt_op_t* instruction)
{
    (void)kt;
    (void)instruction;
}

void kt_halt(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt->halt = 1;
}

void kt_stop(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt->stop = 1;
}

/* ld reg8,imm8 */
void kt_ld_reg8_imm8(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 value;

    value = kt_read(kt, kt->pc + 1);
    kt_set_reg(kt, instruction->dst, value);
}

/* ld reg8,reg8 */
void kt_ld_reg8_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 value;

    value = kt_get_reg(kt, instruction->src);
    kt_set_reg(kt, instruction->dst, value);
}

/* ld reg8,(reg16) */
void kt_ld_reg8_reg16_ind(kouta_t* kt, kt_op_t* instruction)
{
    int addr;
    Uint8 value;

    addr = kt->regs[instruction->src >> 4];
    value = kt_read(kt, addr);
    kt_set_reg(kt, instruction->dst, value);
}

/* ld reg8,(imm16) */
void kt_ld_reg8_imm16_ind(kouta_t* kt, kt_op_t* instruction)
{
    int addr;
    Uint8 value;

    addr = kt_read2(kt, kt->pc + 1);
    value = kt_read(kt, addr);
    kt_set_reg(kt, instruction->dst, value);
}

/* ld reg8,(0xFF00 + himem8) */
void kt_ld_reg8_himem8_ind(kouta_t* kt, kt_op_t* instruction)
{
    int addr;

    addr = 0xFF00 + kt_read(kt, kt->pc + 1);
    kt_set_reg(kt, instruction->dst, kt_read(kt, addr));
}

/* ld (0xFF00 + reg8),reg8 */
void kt_ld_reg8_ind_reg8(kouta_t* kt, kt_op_t* instruction)
{
    int addr;

    addr = 0xFF00 + kt_get_reg(kt, instruction->dst);
    kt_write(kt, addr, kt_get_reg(kt, instruction->src));
}

/* ld reg8,(0xFF00 + reg8) */
void kt_ld_reg8_reg8_ind(kouta_t* kt, kt_op_t* instruction)
{
    int addr;

    addr = 0xFF00 + kt_get_reg(kt, instruction->src);
    kt_set_reg(kt, instruction->dst, kt_read(kt, addr));
}

/* ld (0xFF00 + himem8),reg8 */
void kt_ld_himem8_ind_reg8(kouta_t* kt, kt_op_t* instruction)
{
    int addr;

    addr = 0xFF00 + kt_read(kt, kt->pc + 1);
    kt_write(kt, addr, kt_get_reg(kt, instruction->src));
}

/* ld reg16,imm16 */
void kt_ld_reg16_imm16(kouta_t* kt, kt_op_t* instruction)
{
    kt->regs[instruction->dst >> 4] = kt_read2(kt, kt->pc + 1);
}

/* ld (reg16),reg8 */
void kt_ld_reg16_ind_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    int addr;

    value = (Uint8)kt_get_reg(kt, instruction->src);
    addr = kt->regs[instruction->dst >> 4];
    kt_write(kt, addr, value);
}

/* ld (hl-),reg8 */
void kt_ld_hl_ind_dec_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    int addr;

    value = (Uint8)kt_get_reg(kt, instruction->src);
    addr = kt->regs[KT_HL >> 4];

    kt_write(kt, addr, value);
    --kt->regs[KT_HL >> 4];
}

/* ld (hl+),reg8 */
void kt_ld_hl_ind_inc_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    int addr;

    value = (Uint8)kt_get_reg(kt, instruction->src);
    addr = kt->regs[KT_HL >> 4];

    kt_write(kt, addr, value);
    ++kt->regs[KT_HL >> 4];
}

/* ld reg8,(hl+) */
void kt_ld_reg8_hl_ind_inc(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    int addr;

    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    kt_set_reg(kt, instruction->dst, value);
    ++kt->regs[KT_HL >> 4];
}

/* ld reg8,(hl-) */
void kt_ld_reg8_hl_ind_dec(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    int addr;

    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    kt_set_reg(kt, instruction->dst, value);
    --kt->regs[KT_HL >> 4];
}

/* ld (reg16),imm8 */
void kt_ld_reg16_ind_imm8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    int addr;

    value = kt_read(kt, kt->pc + 1);
    addr = kt->regs[instruction->dst >> 4];
    kt_write(kt, addr, value);
}

/* ld (imm16),reg8 */
void kt_ld_imm16_ind_reg8(kouta_t* kt, kt_op_t* instruction)
{
    int addr;

    addr = kt_read2(kt, kt->pc + 1);
    kt_write(kt, addr, kt_get_reg(kt, instruction->src));
}

/* ld (imm16),sp */
void kt_ld_imm16_ind_sp(kouta_t* kt, kt_op_t* instruction)
{
    int addr;

    (void)instruction;

    addr = kt_read2(kt, kt->pc + 1);
    kt_write2(kt, addr, kt->regs[KT_SP >> 4]);
}

Uint16 kt_add_sp8(kouta_t* kt, Sint8 value)
{
    Uint16 sp;
    Uint16 result;

    sp = kt->regs[KT_SP >> 4];
    result = (Uint16)(sp + value);

    kt->regs[KT_AF >> 4] &= 0xFF00;

    if ((sp ^ value ^ result) & 0x10) {
        kt->regs[KT_AF >> 4] |= KT_HCARRY;
    }

    if (result < value || (result & 0xFF) < (value & 0xFF)) {
        kt->regs[KT_AF >> 4] |= KT_CARRY;
    }

    return result;
}

/* ld hl,sp+sprel8 */
void kt_ld_hl_sprel8(kouta_t* kt, kt_op_t* instruction)
{
    Sint8 rel8;

    (void)instruction;

    rel8 = (Sint8)kt_read(kt, kt->pc + 1);
    kt->regs[KT_HL >> 4] = kt_add_sp8(kt, rel8);
}

/* add sp,simm8 */
void kt_add_sp_simm8(kouta_t* kt, kt_op_t* instruction)
{
    Sint8 simm8;

    (void)instruction;

    simm8 = (Sint8)kt_read(kt, kt->pc + 1);
    kt->regs[KT_SP >> 4] = kt_add_sp8(kt, simm8);
}

/* ld sp,hl */
void kt_ld_sp_hl(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt->regs[KT_SP >> 4] = kt->regs[KT_HL >> 4];
}

/* inc *8 */
Uint8 kt_inc8(kouta_t* kt, Uint8 value)
{
    ++value;

    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_flag(kt, KT_HCARRY, !(value & 0x0F));
    kt->regs[KT_AF >> 4] &= ~KT_SUBTRACT;

    return value;
}

/* inc reg8 */
void kt_inc_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = (Uint8)kt_get_reg(kt, instruction->dst);
    value = kt_inc8(kt, value);
    kt_set_reg(kt, instruction->dst, value);
}

/* inc (hl) */
void kt_inc_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->regs[KT_HL >> 4]);
    value = kt_inc8(kt, value);
    kt_write(kt, kt->regs[KT_HL >> 4], value);
}

/* dec *8 */
Uint8 kt_dec8(kouta_t* kt, Uint8 value)
{
    --value;

    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_flag(kt, KT_HCARRY, (value & 0x0F) == 0x0F);
    kt->regs[KT_AF >> 4] |= KT_SUBTRACT;

    return value;
}

/* dec reg8 */
void kt_dec_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = (Uint8)kt_get_reg(kt, instruction->dst);
    value = kt_dec8(kt, value);
    kt_set_reg(kt, instruction->dst, value);
}

/* dec (hl) */
void kt_dec_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->regs[KT_HL >> 4]);
    value = kt_dec8(kt, value);
    kt_write(kt, kt->regs[KT_HL >> 4], value);
}

/* inc reg16 */
void kt_inc_reg16(kouta_t* kt, kt_op_t* instruction)
{
    ++kt->regs[instruction->dst >> 4];
}

/* dec reg16 */
void kt_dec_reg16(kouta_t* kt, kt_op_t* instruction)
{
    --kt->regs[instruction->dst >> 4];
}

/* scf */
void kt_scf(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt->regs[KT_AF >> 4] |= KT_CARRY;
    kt->regs[KT_AF >> 4] &= ~(KT_SUBTRACT | KT_HCARRY);
}

/* ccf */
void kt_ccf(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_set_flag(kt, KT_CARRY, !(kt->regs[KT_AF >> 4] & KT_CARRY));
    kt->regs[KT_AF >> 4] &= ~(KT_SUBTRACT | KT_HCARRY);
}

/* cp ***8 */
Uint8 kt_cp(kouta_t* kt, Uint16 value)
{
    Uint8 a;
    Uint8 result;

    a = kt_get_reg(kt, KT_A);
    result = (Uint8)(a - value);

    /* this checks if the lowest bit of the high nibble flips */
    kt_set_flag(kt, KT_HCARRY, (a ^ value ^ result) & 0x10);
    kt_set_flag(kt, KT_CARRY, a < value);
    kt_set_flag(kt, KT_ZERO, !result);
    kt->regs[KT_AF >> 4] |= KT_SUBTRACT;

    return result;
}

/* cp imm8 */
void kt_cp_imm8(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_cp(kt, kt_read(kt, kt->pc + 1));
}

/* cp reg8 */
void kt_cp_reg8(kouta_t* kt, kt_op_t* instruction)
{
    kt_cp(kt, kt_get_reg(kt, instruction->dst));
}

/* cp (hl) */
void kt_cp_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_cp(kt, kt_read(kt, kt->regs[KT_HL >> 4]));
}

/* add a,* */
void kt_add_a(kouta_t* kt, Uint16 value)
{
    Uint8 a;
    Uint8 result;

    a = kt_get_reg(kt, KT_A);
    result = (Uint8)(a + value);

    kt->regs[KT_AF >> 4] = 0;
    kt_set_flag(kt, KT_HCARRY, (a ^ value ^ result) & 0x10);
    kt_set_flag(kt, KT_CARRY, (a + value) & 0xFF00);
    kt_set_flag(kt, KT_ZERO, !result);
    kt_set_a(kt, result);
}

/* add a,(hl) */
void kt_add_a_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->regs[KT_HL >> 4]);
    kt_add_a(kt, value);
}

/* add a,reg8 */
void kt_add_a_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->src);
    kt_add_a(kt, value);
}

/* adc a,reg8 */
void kt_adc_a_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 carry;

    value = kt_get_reg(kt, instruction->src);
    carry = kt->regs[KT_AF >> 4];
    carry &= KT_CARRY;
    carry >>= 4;
    kt_add_a(kt, (Uint16)(value + carry));
}

/* adc a,imm8 */
void kt_adc_a_imm8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 carry;

    (void)instruction;

    value = kt_read(kt, kt->pc + 1);
    carry = kt->regs[KT_AF >> 4];
    carry &= KT_CARRY;
    carry >>= 4;
    kt_add_a(kt, (Uint16)(value + carry));
}

/* adc a,(hl) */
void kt_adc_a_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;
    Uint8 carry;

    (void)instruction;
    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    carry = kt->regs[KT_AF >> 4];
    carry &= KT_CARRY;
    carry >>= 4;
    kt_add_a(kt, (Uint16)(value + carry));
}

/* add a,imm8 */
void kt_add_a_imm8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->pc + 1);
    kt_add_a(kt, value);
}

/* add hl,reg16 */
void kt_add_hl_reg16(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 hl;
    Uint16 value;
    Uint16 result;

    hl = kt->regs[KT_HL >> 4];
    value = kt->regs[instruction->src >> 4];
    result = (Uint16)(hl + value);

    kt->regs[KT_AF >> 4] &= ~KT_SUBTRACT;
    kt_set_flag(kt, KT_HCARRY, (hl ^ value ^ result) & 0x1000);
    kt_set_flag(kt, KT_CARRY, result < hl);
    kt->regs[KT_HL >> 4] = result;
}

/* sub reg8 */
void kt_sub_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 result;

    value = kt_get_reg(kt, instruction->dst);
    result = kt_cp(kt, value);
    kt_set_a(kt, result);
}

/* sub imm8 */
void kt_sub_imm8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 result;

    (void)instruction;
    value = kt_read(kt, kt->pc + 1);
    result = kt_cp(kt, value);
    kt_set_a(kt, result);
}

/* sub (hl) */
void kt_sub_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;
    Uint8 result;

    (void)instruction;
    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    result = kt_cp(kt, value);
    kt_set_a(kt, result);
}

/* sbc a,reg8 */
void kt_sbc_a_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 carry;
    Uint8 result;

    value = kt_get_reg(kt, instruction->src);
    carry = kt->regs[KT_AF >> 4];
    carry &= KT_CARRY;
    carry >>= 4;
    result = kt_cp(kt, (Uint16)(value + carry));
    kt_set_a(kt, result);
}

/* sbc a,imm8 */
void kt_sbc_a_imm8(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 value;
    Uint16 carry;
    Uint8 result;

    (void)instruction;
    value = kt_read(kt, kt->pc + 1);
    carry = kt->regs[KT_AF >> 4];
    carry &= KT_CARRY;
    carry >>= 4;
    result = kt_cp(kt, (Uint16)(value + carry));
    kt_set_a(kt, result);
}

/* sbc a,(hl) */
void kt_sbc_a_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;
    Uint8 carry;
    Uint8 result;

    (void)instruction;
    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    carry = kt->regs[KT_AF >> 4];
    carry &= KT_CARRY;
    carry >>= 4;
    result = kt_cp(kt, (Uint16)(value + carry));
    kt_set_a(kt, result);
}

/* or *8 */
void kt_or8(kouta_t* kt, Uint8 value)
{
    Uint16 result;

    result = kt_get_reg(kt, KT_A);
    result |= value;
    kt->regs[KT_AF >> 4] = 0;
    kt_set_a(kt, result);
    kt_set_flag(kt, KT_ZERO, !result);
}

/* or reg8 */
void kt_or_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    kt_or8(kt, value);
}

/* or imm8 */
void kt_or_imm8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->pc + 1);
    kt_or8(kt, value);
}

/* or (hl) */
void kt_or_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->regs[KT_HL >> 4]);
    kt_or8(kt, value);
}

/* xor *8 */
void kt_xor8(kouta_t* kt, Uint8 value)
{
    Uint8 result;

    result = kt_get_reg(kt, KT_A);
    result ^= value;
    kt->regs[KT_AF >> 4] = 0;
    kt_set_a(kt, result);
    kt_set_flag(kt, KT_ZERO, !result);
}

/* xor reg8 */
void kt_xor_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    kt_xor8(kt, value);
}

/* xor imm8 */
void kt_xor_imm8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->pc + 1);
    kt_xor8(kt, value);
}

/* xor (hl) */
void kt_xor_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->regs[KT_HL >> 4]);
    kt_xor8(kt, value);
}

/* and *8 */
void kt_and8(kouta_t* kt, Uint8 value)
{
    Uint16 result;

    result = kt_get_reg(kt, KT_A);
    result &= value;

    kt->regs[KT_AF >> 4] = KT_HCARRY;
    kt_set_a(kt, result);
    kt_set_flag(kt, KT_ZERO, !result);
}

/* and reg8 */
void kt_and_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    kt_and8(kt, value);
}

/* and imm8 */
void kt_and_imm8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->pc + 1);
    kt_and8(kt, value);
}

/* and (hl) */
void kt_and_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;
    value = kt_read(kt, kt->regs[KT_HL >> 4]);
    kt_and8(kt, value);
}

/* daa */
void kt_daa(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 value;
    Uint16 af;

    (void)instruction;

    value = kt_get_reg(kt, KT_A);
    af = kt->regs[KT_AF >> 4];

    /*
     * this is supposed to adjust to decimal representation after
     * operations
     *
     * for example:
     * 0x05 + 0x05 = 0x0A -> 0x10 after daa
     * 0x10 - 0x02 = 0x0E -> 0x08 after daa
     *
     * taken from realboy's implementation atm but i plan on shortening it
     */

    if (af & KT_SUBTRACT)
    {
        if (af & KT_HCARRY) {
            value -= 0x06, value &= 0xFF;
        }

        if (af & KT_CARRY) {
            value -= 0x60;
        }
    }

    else
    {
        if (af & KT_HCARRY || (value & 0x0F) > 0x09) {
            value += 0x06;
        }

        if (af & KT_CARRY || value > 0x9F) {
            value += 0x60;
        }
    }

    if (value & 0x100) {
        kt->regs[KT_AF >> 4] |= KT_CARRY;
    }

    value &= 0xFF;
    kt_set_flag(kt, KT_ZERO, !value);
    kt->regs[KT_AF >> 4] &= ~KT_HCARRY;

    kt_set_reg(kt, KT_A, value);
}

/* jp imm16 */
void kt_jp_imm16(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;

    addr = kt_read2(kt, kt->pc + 1);
    addr -= instruction->size;
    kt->pc = addr;
}

/* jp (hl) */
void kt_jp_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;

    addr = kt->regs[KT_HL >> 4];
    addr -= instruction->size;
    kt->pc = addr;
}

/* jr rel8 */
void kt_jr_rel8(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt->pc += (Sint8)kt_read(kt, kt->pc + 1);
}

/* jr (af & af_mask) == trigger_value,rel8 */
void kt_jr_cond_rel8(kouta_t* kt, int af_mask, int trigger_value)
{
    if ((kt->regs[KT_AF >> 4] & af_mask) != trigger_value) {
        return;
    }

    kt->n_cycles += 4;
    kt->pc += (Sint8)kt_read(kt, kt->pc + 1);
}

/* jr nz,rel8 */
void kt_jr_nz_rel8(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_jr_cond_rel8(kt, KT_ZERO, 0);
}

/* jr z,rel8 */
void kt_jr_z_rel8(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_jr_cond_rel8(kt, KT_ZERO, KT_ZERO);
}

/* jr nc,rel8 */
void kt_jr_nc_rel8(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_jr_cond_rel8(kt, KT_CARRY, 0);
}

/* jr c,rel8 */
void kt_jr_c_rel8(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_jr_cond_rel8(kt, KT_CARRY, KT_CARRY);
}

/* jp (af & af_mask) == trigger_value,imm16 */
void kt_jp_cond_imm16(kouta_t* kt, int af_mask, int trigger_value,
    kt_op_t* instruction)
{
    if ((kt->regs[KT_AF >> 4] & af_mask) != trigger_value) {
        return;
    }

    kt->n_cycles += 4;
    kt->pc = kt_read2(kt, kt->pc + 1) - instruction->size;
}

/* jp z,imm16 */
void kt_jp_z_imm16(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_jp_cond_imm16(kt, KT_ZERO, KT_ZERO, instruction);
}

/* jp nz,imm16 */
void kt_jp_nz_imm16(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_jp_cond_imm16(kt, KT_ZERO, 0, instruction);
}

/* jp c,imm16 */
void kt_jp_c_imm16(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_jp_cond_imm16(kt, KT_CARRY, KT_CARRY, instruction);
}

/* jp nc,imm16 */
void kt_jp_nc_imm16(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt_jp_cond_imm16(kt, KT_CARRY, 0, instruction);
}

/* di */
void kt_di(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt->ime_events = KT_IME_DI;
}

/* ei */
void kt_ei(kouta_t* kt, kt_op_t* instruction)
{
    (void)instruction;
    kt->ime_events = KT_IME_EI;
}

/* cpl */
void kt_cpl(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    (void)instruction;

    value = kt_get_reg(kt, KT_A);
    value = ~value;

    kt->regs[KT_AF >> 4] |= KT_SUBTRACT;
    kt->regs[KT_AF >> 4] |= KT_HCARRY;

    kt_set_reg(kt, KT_A, value);
}

/* rst lomem */
void kt_rst_lomem(kouta_t* kt, kt_op_t* instruction)
{
    /*
     * manual says it pushes the "present address", but in reality the cpu
     * already fetched the instruction meaning that we push the addr
     * AFTER the rst instruction
     */

    kt_push2(kt, kt->pc + 1);
    kt->pc = instruction->dst - instruction->size;
}

/* call imm16 */
void kt_call_imm16(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;

    addr = kt_read2(kt, kt->pc + 1);
    addr -= instruction->size;
    kt_push2(kt, kt->pc + instruction->size);
    kt->pc = addr;
}

/* call (af & af_mask) == trigger_value,imm16 */
void kt_call_cond_imm16(kouta_t* kt, int af_mask, int trigger_value,
    kt_op_t* instruction)
{
    if ((kt->regs[KT_AF >> 4] & af_mask) != trigger_value) {
        return;
    }

    kt->n_cycles += 12;
    kt_call_imm16(kt, instruction);
}

/* call z imm16 */
void kt_call_z_imm16(kouta_t* kt, kt_op_t* instruction)
{
    kt_call_cond_imm16(kt, KT_ZERO, KT_ZERO, instruction);
}

/* call nz imm16 */
void kt_call_nz_imm16(kouta_t* kt, kt_op_t* instruction)
{
    kt_call_cond_imm16(kt, KT_ZERO, 0, instruction);
}

/* call c imm16 */
void kt_call_c_imm16(kouta_t* kt, kt_op_t* instruction)
{
    kt_call_cond_imm16(kt, KT_CARRY, KT_CARRY, instruction);
}

/* call nc imm16 */
void kt_call_nc_imm16(kouta_t* kt, kt_op_t* instruction)
{
    kt_call_cond_imm16(kt, KT_CARRY, 0, instruction);
}

/* ret */
void kt_ret(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;

    addr = kt_pop2(kt);
    addr -= instruction->size;
    kt->pc = addr;
}

/* ret nz */
void kt_ret_nz(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;

    if (kt->regs[KT_AF >> 4] & KT_ZERO) {
        return;
    }

    addr = kt_pop2(kt);
    addr -= instruction->size;
    kt->pc = addr;
    kt->n_cycles += 12;
}

/* ret z */
void kt_ret_z(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;

    if (kt->regs[KT_AF >> 4] & KT_ZERO)
    {
        addr = kt_pop2(kt);
        addr -= instruction->size;
        kt->pc = addr;
        kt->n_cycles += 12;
    }
}

/* ret nc */
void kt_ret_nc(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;

    if (kt->regs[KT_AF >> 4] & KT_CARRY) {
        return;
    }

    addr = kt_pop2(kt);
    addr -= instruction->size;
    kt->pc = addr;
    kt->n_cycles += 12;
}

/* ret c */
void kt_ret_c(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;

    if (kt->regs[KT_AF >> 4] & KT_CARRY)
    {
        addr = kt_pop2(kt);
        addr -= instruction->size;
        kt->pc = addr;
        kt->n_cycles += 12;
    }
}

/* reti */
void kt_reti(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;

    addr = kt_pop2(kt);
    addr -= instruction->size;
    kt->pc = addr;
    kt->ime = 1;
}

/* push reg16 */
void kt_push_reg16(kouta_t* kt, kt_op_t* instruction)
{
    kt_push2(kt, kt->regs[instruction->dst >> 4]);
}

/* pop reg16 */
void kt_pop_reg16(kouta_t* kt, kt_op_t* instruction)
{
    kt->regs[instruction->dst >> 4] = kt_pop2(kt);
}

/* pop af */
void kt_pop_af(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 value;

    (void)instruction;

    value = kt_pop2(kt);
    value &= 0xFFF0;

    kt->regs[KT_AF >> 4] &= 0x000F;
    kt->regs[KT_AF >> 4] |= value;
}

/* set n,reg8 */
void kt_set_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    value |= 1 << instruction->src;
    kt_set_reg(kt, instruction->dst, value);
}

/* set n,(hl) */
void kt_set_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;

    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    value |= 1 << instruction->src;
    kt_write(kt, addr, value);
}

/* bit n,*8 */
void kt_bit8(kouta_t* kt, int n, Uint8 value)
{
    kt_set_flag(kt, KT_ZERO, !(value & (1 << n)));
    kt->regs[KT_AF >> 4] &= ~KT_SUBTRACT;
    kt->regs[KT_AF >> 4] |= KT_HCARRY;
}

/* bit n,reg8 */
void kt_bit_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    kt_bit8(kt, instruction->src, value);
}

/* bit n,(hl) */
void kt_bit_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 address;
    Uint8 value;

    address = kt->regs[KT_HL >> 4];
    value = kt_read(kt, address);
    kt_bit8(kt, instruction->src, value);
}

/* res n,reg8 */
void kt_res_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    value &= ~(1 << instruction->src);
    kt_set_reg(kt, instruction->dst, value);
}

/* res n,(hl) */
void kt_res_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 address;
    Uint8 value;

    address = kt->regs[KT_HL >> 4];
    value = kt_read(kt, address);
    value &= ~(1 << instruction->src);
    kt_write(kt, address, value);
}

/* rl *8 */
Uint8 kt_rl8(kouta_t* kt, Uint8 value)
{
    Uint8 old;

    old = value;
    value <<= 1;

    if (kt->regs[KT_AF >> 4] & KT_CARRY) {
        value |= 1;
    } else {
        value &= ~1;
    }

    kt->regs[KT_AF >> 4] &= 0xFF00;
    kt_set_flag(kt, KT_CARRY, old & 0x80);
    kt_set_flag(kt, KT_ZERO, !value);

    return value;
}

/* rl reg8 */
void kt_rl_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = (Uint8)kt_get_reg(kt, instruction->dst);
    value = kt_rl8(kt, value);
    kt_set_reg(kt, instruction->dst, value);
}

/* rl (hl) */
void kt_rl_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;

    (void)instruction;
    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    value = kt_rl8(kt, value);
    kt_write(kt, addr, value);
}

/* rrca */
void kt_rrca(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 old_value;

    (void)instruction;

    value = (Uint8)kt_get_reg(kt, KT_A);
    old_value = value;
    value >>= 1;

    if (old_value & 1) {
        value |= 0x80;
    } else {
        value &= ~0x80;
    }

    kt->regs[KT_AF >> 4] = 0;
    kt_set_flag(kt, KT_CARRY, old_value & 1);
    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_a(kt, value);
}

/* rra */
void kt_rra(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 old_value;

    (void)instruction;

    value = (Uint8)kt_get_reg(kt, KT_A);
    old_value = value;
    value >>= 1;

    if (kt->regs[KT_AF >> 4] & KT_CARRY) {
        value |= 0x80;
    } else {
        value &= ~0x80;
    }

    kt->regs[KT_AF >> 4] = 0;
    kt_set_flag(kt, KT_CARRY, old_value & 1);
    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_a(kt, value);
}

Uint8 kt_rr8(kouta_t* kt, Uint8 value)
{
    Uint8 old;

    old = value;
    value >>= 1;

    if (kt->regs[KT_AF >> 4] & KT_CARRY) {
        value |= 0x80;
    } else {
        value &= ~0x80;
    }

    kt->regs[KT_AF >> 4] &= 0xFF00;
    kt_set_flag(kt, KT_CARRY, old & 1);
    kt_set_flag(kt, KT_ZERO, !value);

    return value;
}

/* rr reg8 */
void kt_rr_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = (Uint8)kt_get_reg(kt, instruction->dst);
    value = kt_rr8(kt, value);
    kt_set_reg(kt, instruction->dst, value);
}

/* rr (hl) */
void kt_rr_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;

    (void)instruction;
    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    value = kt_rr8(kt, value);
    kt_write(kt, addr, value);
}

/* rrc *8 */
Uint8 kt_rrc8(kouta_t* kt, Uint8 value)
{
    Uint8 old;

    old = value;
    value >>= 1;

    if (old & 1) {
        value |= 0x80;
    } else {
        value &= ~0x80;
    }

    kt->regs[KT_AF >> 4] &= 0xFF00;
    kt_set_flag(kt, KT_CARRY, old & 1);
    kt_set_flag(kt, KT_ZERO, !value);

    return value;
}

/* rrc (hl) */
void kt_rrc_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;

    (void)instruction;
    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    value = kt_rrc8(kt, value);
    kt_write(kt, addr, value);
}

/* rrc reg8 */
void kt_rrc_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    value = kt_rrc8(kt, value);
    kt_set_reg(kt, instruction->dst, value);
}

/* rla */
void kt_rla(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 old_value;

    (void)instruction;

    value = (Uint8)kt_get_reg(kt, KT_A);
    old_value = value;
    value <<= 1;

    if (kt->regs[KT_AF >> 4] & KT_CARRY) {
        value |= 1;
    } else {
        value &= ~1;
    }

    /*
     * there's inconsistencies in the official cpu manual, but it seems
     * like the correct behaviour is to always clear the zero flag
     * https://hax.iimarckus.org/topic/1617/
     */

    kt->regs[KT_AF >> 4] = 0;
    kt_set_flag(kt, KT_CARRY, old_value & 0x80);
    kt_set_a(kt, value);
}

/* rlc *8 */
Uint8 kt_rlc8(kouta_t* kt, Uint8 value)
{
    Uint8 old;

    old = value;
    value <<= 1;

    if (old & 0x80) {
        value |= 1;
    } else {
        value &= ~1;
    }

    kt->regs[KT_AF >> 4] &= 0xFF00;
    kt_set_flag(kt, KT_CARRY, old & 1);
    kt_set_flag(kt, KT_ZERO, !value);

    return value;
}

/* rlc reg8 */
void kt_rlc_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    kt_set_reg(kt, instruction->dst, kt_rlc8(kt, value));
}

/* rlc (hl) */
void kt_rlc_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;

    (void)instruction;
    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    kt_write(kt, addr, kt_rlc8(kt, value));
}

/* rlca */
void kt_rlca(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 old_value;

    (void)instruction;

    value = (Uint8)kt_get_reg(kt, KT_A);
    old_value = value;
    value <<= 1;

    if (old_value & 0x80) {
        value |= 1;
    } else {
        value &= ~1;
    }

    kt->regs[KT_AF >> 4] = 0;
    kt_set_flag(kt, KT_CARRY, old_value & 0x80);
    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_a(kt, value);
}

/* sla reg8 */
void kt_sla_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;
    Uint8 old_value;

    value = kt_get_reg(kt, instruction->dst);
    old_value = value;
    value <<= 1;
    value &= 0xFE;

    kt->regs[KT_AF >> 4] &= 0xFF00;
    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_flag(kt, KT_CARRY, old_value & 0x80);
    kt_set_reg(kt, instruction->dst, value);
}

/* sla (hl) */
void kt_sla_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;
    Uint8 old_value;

    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    old_value = value;
    value <<= 1;
    value &= 0xFE;

    kt->regs[KT_AF >> 4] &= 0xFF00;
    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_flag(kt, KT_CARRY, old_value & 0x80);
    kt_set_reg(kt, instruction->dst, value);
}

/* srl *8 */
Uint8 kt_srl8(kouta_t* kt, Uint8 value)
{
    Uint8 old_value;

    old_value = value;
    value >>= 1;
    value &= 0x7F;

    kt->regs[KT_AF >> 4] &= 0xFF00;
    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_flag(kt, KT_CARRY, old_value & 1);

    return value;
}

/* srl reg8 */
void kt_srl_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    value = kt_srl8(kt, value);
    kt_set_reg(kt, instruction->dst, value);
}

/* srl (hl) */
void kt_srl_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;

    (void)instruction;
    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    value = kt_srl8(kt, value);
    kt_write(kt, addr, value);
}

/* sra *8 */
Uint8 kt_sra8(kouta_t* kt, Uint8 value)
{
    Uint8 old_value;

    old_value = value;
    value >>= 1;
    value |= old_value & 0x80;

    kt->regs[KT_AF >> 4] &= 0xFF00;
    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_flag(kt, KT_CARRY, old_value & 1);

    return value;
}

/* sra reg8 */
void kt_sra_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);
    value = kt_sra8(kt, value);
    kt_set_reg(kt, instruction->dst, value);
}

/* sra (hl) */
void kt_sra_hl_ind(kouta_t* kt, kt_op_t* instruction)
{
    Uint16 addr;
    Uint8 value;

    (void)instruction;
    addr = kt->regs[KT_HL >> 4];
    value = kt_read(kt, addr);
    value = kt_sra8(kt, value);
    kt_write(kt, addr, value);
}

/* swap reg8 */
void kt_swap_reg8(kouta_t* kt, kt_op_t* instruction)
{
    Uint8 value;

    value = kt_get_reg(kt, instruction->dst);

    value = (Uint8)(
        ((value << 4) & 0xF0) |
        ((value >> 4) & 0x0F)
    );

    kt->regs[KT_AF >> 4] &= 0xFF00;
    kt_set_flag(kt, KT_ZERO, !value);
    kt_set_reg(kt, instruction->dst, value);
}

/* --------------------------------------------------------------------- */

enum kt_addressing_mode
{
    KT_NULL_ADDRESSING_MODE,
    KT_REG,
    KT_REG_IND,
    KT_REG_IND_DEC,
    KT_REG_IND_INC,
    KT_REL8,
    KT_IMM8,
    KT_SIMM8,
    KT_LOMEM,
    KT_HIMEM8,
    KT_HIMEM8_IND,
    KT_SPREL8,
    KT_IMM16,
    KT_IMM16_IND,
    KT_BIT,
    KT_LAST_ADDRESSING_MODE
};

kt_op_t kt_op_table[512] = {
    { 0x00, "NOP", 0, 0, 0, 0, kt_nop, 1, 4 },
    { 0x01, "LD", KT_REG, KT_BC, KT_IMM16, 0, kt_ld_reg16_imm16, 3, 12 },
    { 0x02, "LD", KT_REG_IND, KT_BC, KT_REG, KT_A,
        kt_ld_reg16_ind_reg8, 1, 8 },
    { 0x03, "INC", KT_REG, KT_BC, 0, 0, kt_inc_reg16, 1, 8 },
    { 0x04, "INC", KT_REG, KT_B, 0, 0, kt_inc_reg8, 1, 4 },
    { 0x05, "DEC", KT_REG, KT_B, 0, 0, kt_dec_reg8, 1, 4 },
    { 0x06, "LD", KT_REG, KT_B, KT_IMM8, 0, kt_ld_reg8_imm8, 2, 8 },
    { 0x07, "RLCA", 0, 0, 0, 0, kt_rlca, 1, 4 },
    { 0x08, "LD", KT_IMM16_IND, 0, KT_REG, KT_SP,
        kt_ld_imm16_ind_sp, 3, 20 },
    { 0x09, "ADD", KT_REG, KT_HL, KT_REG, KT_BC, kt_add_hl_reg16, 1, 8 },
    { 0x0A, "LD", KT_REG, KT_A, KT_REG_IND, KT_BC,
        kt_ld_reg8_reg16_ind, 1, 8 },
    { 0x0B, "DEC", KT_REG, KT_BC, 0, 0, kt_dec_reg16, 1, 8 },
    { 0x0C, "INC", KT_REG, KT_C, 0, 0, kt_inc_reg8, 1, 4 },
    { 0x0D, "DEC", KT_REG, KT_C, 0, 0, kt_dec_reg8, 1, 4 },
    { 0x0E, "LD", KT_REG, KT_C, KT_IMM8, 0, kt_ld_reg8_imm8, 2, 8 },
    { 0x0F, "RRCA", 0, 0, 0, 0, kt_rrca, 1, 4 },
    { 0x10, "STOP", 0, 0, 0, 0, kt_stop, 2, 4 },
    { 0x11, "LD", KT_REG, KT_DE, KT_IMM16, 0, kt_ld_reg16_imm16, 3, 12 },
    { 0x12, "LD", KT_REG_IND, KT_DE, KT_REG, KT_A,
        kt_ld_reg16_ind_reg8, 1, 8 },
    { 0x13, "INC", KT_REG, KT_DE, 0, 0, kt_inc_reg16, 1, 8 },
    { 0x14, "INC", KT_REG, KT_D, 0, 0, kt_inc_reg8, 1, 4 },
    { 0x15, "DEC", KT_REG, KT_D, 0, 0, kt_dec_reg8, 1, 4 },
    { 0x16, "LD", KT_REG, KT_D, KT_IMM8, 0, kt_ld_reg8_imm8, 2, 8 },
    { 0x17, "RLA", 0, 0, 0, 0, kt_rla, 1, 4 },
    { 0x18, "JR", KT_REL8, 0, 0, 0, kt_jr_rel8, 2, 12 },
    { 0x19, "ADD", KT_REG, KT_HL, KT_REG, KT_DE, kt_add_hl_reg16, 1, 8 },
    { 0x1A, "LD", KT_REG, KT_A, KT_REG_IND, KT_DE,
        kt_ld_reg8_reg16_ind, 1, 8 },
    { 0x1B, "DEC", KT_REG, KT_DE, 0, 0, kt_dec_reg16, 1, 8 },
    { 0x1C, "INC", KT_REG, KT_E, 0, 0, kt_inc_reg8, 1, 4 },
    { 0x1D, "DEC", KT_REG, KT_E, 0, 0, kt_dec_reg8, 1, 4 },
    { 0x1E, "LD", KT_REG, KT_E, KT_IMM8, 0, kt_ld_reg8_imm8, 2, 8 },
    { 0x1F, "RRA", 0, 0, 0, 0, kt_rra, 1, 4 },
    { 0x20, "JR NZ", KT_REL8, 0, 0, 0, kt_jr_nz_rel8, 2, 8 },
    { 0x21, "LD", KT_REG, KT_HL, KT_IMM16, 0, kt_ld_reg16_imm16, 3, 12 },
    { 0x22, "LD", KT_REG_IND_INC, KT_HL, KT_REG, KT_A,
        kt_ld_hl_ind_inc_reg8, 1, 8 },
    { 0x23, "INC", KT_REG, KT_HL, 0, 0, kt_inc_reg16, 1, 8 },
    { 0x24, "INC", KT_REG, KT_H, 0, 0, kt_inc_reg8, 1, 4 },
    { 0x25, "DEC", KT_REG, KT_H, 0, 0, kt_dec_reg8, 1, 4 },
    { 0x26, "LD", KT_REG, KT_H, KT_IMM8, 0, kt_ld_reg8_imm8, 2, 8 },
    { 0x27, "DAA", 0, 0, 0, 0, kt_daa, 1, 4 },
    { 0x28, "JR Z", KT_REL8, 0, 0, 0, kt_jr_z_rel8, 2, 8 },
    { 0x29, "ADD", KT_REG, KT_HL, KT_REG, KT_HL, kt_add_hl_reg16, 1, 8 },
    { 0x2A, "LD", KT_REG, KT_A, KT_REG_IND_INC, KT_HL,
        kt_ld_reg8_hl_ind_inc, 1, 8 },
    { 0x2B, "DEC", KT_REG, KT_HL, 0, 0, kt_dec_reg16, 1, 8 },
    { 0x2C, "INC", KT_REG, KT_L, 0, 0, kt_inc_reg8, 1, 4 },
    { 0x2D, "DEC", KT_REG, KT_L, 0, 0, kt_dec_reg8, 1, 4 },
    { 0x2E, "LD", KT_REG, KT_L, KT_IMM8, 0, kt_ld_reg8_imm8, 2, 8 },
    { 0x2F, "CPL", 0, 0, 0, 0, kt_cpl, 1, 4 },
    { 0x30, "JR NC", KT_REL8, 0, 0, 0, kt_jr_nc_rel8, 2, 8 },
    { 0x31, "LD", KT_REG, KT_SP, KT_IMM16, 0, kt_ld_reg16_imm16, 3, 12 },
    { 0x32, "LD", KT_REG_IND_DEC, KT_HL, KT_REG, KT_A,
        kt_ld_hl_ind_dec_reg8, 1, 8 },
    { 0x33, "INC", KT_REG, KT_SP, 0, 0, kt_inc_reg16, 1, 8 },
    { 0x34, "INC", KT_REG_IND, KT_HL, 0, 0, kt_inc_hl_ind, 1, 12 },
    { 0x35, "DEC", KT_REG_IND, KT_HL, 0, 0, kt_dec_hl_ind, 1, 12 },
    { 0x36, "LD", KT_REG_IND, KT_HL, KT_IMM8, 0,
        kt_ld_reg16_ind_imm8, 2, 12 },
    { 0x37, "SCF", 0, 0, 0, 0, kt_scf, 1, 4 },
    { 0x38, "JR C", KT_REL8, 0, 0, 0, kt_jr_c_rel8, 2, 8 },
    { 0x39, "ADD", KT_REG, KT_HL, KT_REG, KT_SP, kt_add_hl_reg16, 1, 8 },
    { 0x3A, "LD", KT_REG, KT_A, KT_REG_IND_DEC, KT_HL,
        kt_ld_reg8_hl_ind_dec, 1, 8 },
    { 0x3B, "DEC", KT_REG, KT_SP, 0, 0, kt_dec_reg16, 1, 8 },
    { 0x3C, "INC", KT_REG, KT_A, 0, 0, kt_inc_reg8, 1, 4 },
    { 0x3D, "DEC", KT_REG, KT_A, 0, 0, kt_dec_reg8, 1, 4 },
    { 0x3E, "LD", KT_REG, KT_A, KT_IMM8, 0, kt_ld_reg8_imm8, 2, 8 },
    { 0x3F, "CCF", 0, 0, 0, 0, kt_ccf, 1, 4 },
    { 0x40, "LD", KT_REG, KT_B, KT_REG, KT_B, kt_ld_reg8_reg8, 1, 4 },
    { 0x41, "LD", KT_REG, KT_B, KT_REG, KT_C, kt_ld_reg8_reg8, 1, 4 },
    { 0x42, "LD", KT_REG, KT_B, KT_REG, KT_D, kt_ld_reg8_reg8, 1, 4 },
    { 0x43, "LD", KT_REG, KT_B, KT_REG, KT_E, kt_ld_reg8_reg8, 1, 4 },
    { 0x44, "LD", KT_REG, KT_B, KT_REG, KT_H, kt_ld_reg8_reg8, 1, 4 },
    { 0x45, "LD", KT_REG, KT_B, KT_REG, KT_L, kt_ld_reg8_reg8, 1, 4 },
    { 0x46, "LD", KT_REG, KT_B, KT_REG_IND, KT_HL,
        kt_ld_reg8_reg16_ind, 1, 8 },
    { 0x47, "LD", KT_REG, KT_B, KT_REG, KT_A, kt_ld_reg8_reg8, 1, 4 },
    { 0x48, "LD", KT_REG, KT_C, KT_REG, KT_B, kt_ld_reg8_reg8, 1, 4 },
    { 0x49, "LD", KT_REG, KT_C, KT_REG, KT_C, kt_ld_reg8_reg8, 1, 4 },
    { 0x4A, "LD", KT_REG, KT_C, KT_REG, KT_D, kt_ld_reg8_reg8, 1, 4 },
    { 0x4B, "LD", KT_REG, KT_C, KT_REG, KT_E, kt_ld_reg8_reg8, 1, 4 },
    { 0x4C, "LD", KT_REG, KT_C, KT_REG, KT_H, kt_ld_reg8_reg8, 1, 4 },
    { 0x4D, "LD", KT_REG, KT_C, KT_REG, KT_L, kt_ld_reg8_reg8, 1, 4 },
    { 0x4E, "LD", KT_REG, KT_C, KT_REG_IND, KT_HL,
        kt_ld_reg8_reg16_ind, 1, 8 },
    { 0x4F, "LD", KT_REG, KT_C, KT_REG, KT_A, kt_ld_reg8_reg8, 1, 4 },
    { 0x50, "LD", KT_REG, KT_D, KT_REG, KT_B, kt_ld_reg8_reg8, 1, 4 },
    { 0x51, "LD", KT_REG, KT_D, KT_REG, KT_C, kt_ld_reg8_reg8, 1, 4 },
    { 0x52, "LD", KT_REG, KT_D, KT_REG, KT_D, kt_ld_reg8_reg8, 1, 4 },
    { 0x53, "LD", KT_REG, KT_D, KT_REG, KT_E, kt_ld_reg8_reg8, 1, 4 },
    { 0x54, "LD", KT_REG, KT_D, KT_REG, KT_H, kt_ld_reg8_reg8, 1, 4 },
    { 0x55, "LD", KT_REG, KT_D, KT_REG, KT_L, kt_ld_reg8_reg8, 1, 4 },
    { 0x56, "LD", KT_REG, KT_D, KT_REG_IND, KT_HL,
        kt_ld_reg8_reg16_ind, 1, 8 },
    { 0x57, "LD", KT_REG, KT_D, KT_REG, KT_A, kt_ld_reg8_reg8, 1, 4 },
    { 0x58, "LD", KT_REG, KT_E, KT_REG, KT_B, kt_ld_reg8_reg8, 1, 4 },
    { 0x59, "LD", KT_REG, KT_E, KT_REG, KT_C, kt_ld_reg8_reg8, 1, 4 },
    { 0x5A, "LD", KT_REG, KT_E, KT_REG, KT_D, kt_ld_reg8_reg8, 1, 4 },
    { 0x5B, "LD", KT_REG, KT_E, KT_REG, KT_E, kt_ld_reg8_reg8, 1, 4 },
    { 0x5C, "LD", KT_REG, KT_E, KT_REG, KT_H, kt_ld_reg8_reg8, 1, 4 },
    { 0x5D, "LD", KT_REG, KT_E, KT_REG, KT_L, kt_ld_reg8_reg8, 1, 4 },
    { 0x5E, "LD", KT_REG, KT_E, KT_REG_IND, KT_HL,
        kt_ld_reg8_reg16_ind, 1, 8 },
    { 0x5F, "LD", KT_REG, KT_E, KT_REG, KT_A, kt_ld_reg8_reg8, 1, 4 },
    { 0x60, "LD", KT_REG, KT_H, KT_REG, KT_B, kt_ld_reg8_reg8, 1, 4 },
    { 0x61, "LD", KT_REG, KT_H, KT_REG, KT_C, kt_ld_reg8_reg8, 1, 4 },
    { 0x62, "LD", KT_REG, KT_H, KT_REG, KT_D, kt_ld_reg8_reg8, 1, 4 },
    { 0x63, "LD", KT_REG, KT_H, KT_REG, KT_E, kt_ld_reg8_reg8, 1, 4 },
    { 0x64, "LD", KT_REG, KT_H, KT_REG, KT_H, kt_ld_reg8_reg8, 1, 4 },
    { 0x65, "LD", KT_REG, KT_H, KT_REG, KT_L, kt_ld_reg8_reg8, 1, 4 },
    { 0x66, "LD", KT_REG, KT_H, KT_REG_IND, KT_HL,
        kt_ld_reg8_reg16_ind, 1, 8 },
    { 0x67, "LD", KT_REG, KT_H, KT_REG, KT_A, kt_ld_reg8_reg8, 1, 4 },
    { 0x68, "LD", KT_REG, KT_L, KT_REG, KT_B, kt_ld_reg8_reg8, 1, 4 },
    { 0x69, "LD", KT_REG, KT_L, KT_REG, KT_C, kt_ld_reg8_reg8, 1, 4 },
    { 0x6A, "LD", KT_REG, KT_L, KT_REG, KT_D, kt_ld_reg8_reg8, 1, 4 },
    { 0x6B, "LD", KT_REG, KT_L, KT_REG, KT_E, kt_ld_reg8_reg8, 1, 4 },
    { 0x6C, "LD", KT_REG, KT_L, KT_REG, KT_H, kt_ld_reg8_reg8, 1, 4 },
    { 0x6D, "LD", KT_REG, KT_L, KT_REG, KT_L, kt_ld_reg8_reg8, 1, 4 },
    { 0x6E, "LD", KT_REG, KT_L, KT_REG_IND, KT_HL,
        kt_ld_reg8_reg16_ind, 1, 8 },
    { 0x6F, "LD", KT_REG, KT_L, KT_REG, KT_A, kt_ld_reg8_reg8, 1, 4 },
    { 0x70, "LD", KT_REG_IND, KT_HL, KT_REG, KT_B,
        kt_ld_reg16_ind_reg8, 1, 8 },
    { 0x71, "LD", KT_REG_IND, KT_HL, KT_REG, KT_C,
        kt_ld_reg16_ind_reg8, 1, 8 },
    { 0x72, "LD", KT_REG_IND, KT_HL, KT_REG, KT_D,
        kt_ld_reg16_ind_reg8, 1, 8 },
    { 0x73, "LD", KT_REG_IND, KT_HL, KT_REG, KT_E,
        kt_ld_reg16_ind_reg8, 1, 8 },
    { 0x74, "LD", KT_REG_IND, KT_HL, KT_REG, KT_H,
        kt_ld_reg16_ind_reg8, 1, 8 },
    { 0x75, "LD", KT_REG_IND, KT_HL, KT_REG, KT_L,
        kt_ld_reg16_ind_reg8, 1, 8 },
    { 0x76, "HALT", 0, 0, 0, 0, kt_halt, 1, 4 },
    { 0x77, "LD", KT_REG_IND, KT_HL, KT_REG, KT_A,
        kt_ld_reg16_ind_reg8, 1, 8 },
    { 0x78, "LD", KT_REG, KT_A, KT_REG, KT_B, kt_ld_reg8_reg8, 1, 4 },
    { 0x79, "LD", KT_REG, KT_A, KT_REG, KT_C, kt_ld_reg8_reg8, 1, 4 },
    { 0x7A, "LD", KT_REG, KT_A, KT_REG, KT_D, kt_ld_reg8_reg8, 1, 4 },
    { 0x7B, "LD", KT_REG, KT_A, KT_REG, KT_E, kt_ld_reg8_reg8, 1, 4 },
    { 0x7C, "LD", KT_REG, KT_A, KT_REG, KT_H, kt_ld_reg8_reg8, 1, 4 },
    { 0x7D, "LD", KT_REG, KT_A, KT_REG, KT_L, kt_ld_reg8_reg8, 1, 4 },
    { 0x7E, "LD", KT_REG, KT_A, KT_REG_IND, KT_HL,
        kt_ld_reg8_reg16_ind, 1, 8 },
    { 0x7F, "LD", KT_REG, KT_A, KT_REG, KT_A, kt_ld_reg8_reg8, 1, 4 },
    { 0x80, "ADD", KT_REG, KT_A, KT_REG, KT_B, kt_add_a_reg8, 1, 4 },
    { 0x81, "ADD", KT_REG, KT_A, KT_REG, KT_C, kt_add_a_reg8, 1, 4 },
    { 0x82, "ADD", KT_REG, KT_A, KT_REG, KT_D, kt_add_a_reg8, 1, 4 },
    { 0x83, "ADD", KT_REG, KT_A, KT_REG, KT_E, kt_add_a_reg8, 1, 4 },
    { 0x84, "ADD", KT_REG, KT_A, KT_REG, KT_H, kt_add_a_reg8, 1, 4 },
    { 0x85, "ADD", KT_REG, KT_A, KT_REG, KT_L, kt_add_a_reg8, 1, 4 },
    { 0x86, "ADD", KT_REG, KT_A, KT_REG_IND, KT_HL,
        kt_add_a_hl_ind, 1, 8 },
    { 0x87, "ADD", KT_REG, KT_A, KT_REG, KT_A, kt_add_a_reg8, 1, 4 },
    { 0x88, "ADC", KT_REG, KT_A, KT_REG, KT_B, kt_adc_a_reg8, 1, 4 },
    { 0x89, "ADC", KT_REG, KT_A, KT_REG, KT_C, kt_adc_a_reg8, 1, 4 },
    { 0x8A, "ADC", KT_REG, KT_A, KT_REG, KT_D, kt_adc_a_reg8, 1, 4 },
    { 0x8B, "ADC", KT_REG, KT_A, KT_REG, KT_E, kt_adc_a_reg8, 1, 4 },
    { 0x8C, "ADC", KT_REG, KT_A, KT_REG, KT_H, kt_adc_a_reg8, 1, 4 },
    { 0x8D, "ADC", KT_REG, KT_A, KT_REG, KT_L, kt_adc_a_reg8, 1, 4 },
    { 0x8E, "ADC", KT_REG, KT_A, KT_REG_IND, KT_HL,
        kt_adc_a_hl_ind, 1, 8 },
    { 0x8F, "ADC", KT_REG, KT_A, KT_REG, KT_A, kt_adc_a_reg8, 1, 4 },
    { 0x90, "SUB", KT_REG, KT_B, 0, 0, kt_sub_reg8, 1, 4 },
    { 0x91, "SUB", KT_REG, KT_C, 0, 0, kt_sub_reg8, 1, 4 },
    { 0x92, "SUB", KT_REG, KT_D, 0, 0, kt_sub_reg8, 1, 4 },
    { 0x93, "SUB", KT_REG, KT_E, 0, 0, kt_sub_reg8, 1, 4 },
    { 0x94, "SUB", KT_REG, KT_H, 0, 0, kt_sub_reg8, 1, 4 },
    { 0x95, "SUB", KT_REG, KT_L, 0, 0, kt_sub_reg8, 1, 4 },
    { 0x96, "SUB", KT_REG_IND, KT_HL, 0, 0, kt_sub_hl_ind, 1, 8 },
    { 0x97, "SUB", KT_REG, KT_A, 0, 0, kt_sub_reg8, 1, 4 },
    { 0x98, "SBC", KT_REG, KT_A, KT_REG, KT_B, kt_sbc_a_reg8, 1, 4 },
    { 0x99, "SBC", KT_REG, KT_A, KT_REG, KT_C, kt_sbc_a_reg8, 1, 4 },
    { 0x9A, "SBC", KT_REG, KT_A, KT_REG, KT_D, kt_sbc_a_reg8, 1, 4 },
    { 0x9B, "SBC", KT_REG, KT_A, KT_REG, KT_E, kt_sbc_a_reg8, 1, 4 },
    { 0x9C, "SBC", KT_REG, KT_A, KT_REG, KT_H, kt_sbc_a_reg8, 1, 4 },
    { 0x9D, "SBC", KT_REG, KT_A, KT_REG, KT_L, kt_sbc_a_reg8, 1, 4 },
    { 0x9E, "SBC", KT_REG, KT_A, KT_REG_IND, KT_HL,
        kt_sbc_a_hl_ind, 1, 8 },
    { 0x9F, "SBC", KT_REG, KT_A, KT_REG, KT_A, kt_sbc_a_reg8, 1, 4 },
    { 0xA0, "AND", KT_REG, KT_B, 0, 0, kt_and_reg8, 1, 4 },
    { 0xA1, "AND", KT_REG, KT_C, 0, 0, kt_and_reg8, 1, 4 },
    { 0xA2, "AND", KT_REG, KT_D, 0, 0, kt_and_reg8, 1, 4 },
    { 0xA3, "AND", KT_REG, KT_E, 0, 0, kt_and_reg8, 1, 4 },
    { 0xA4, "AND", KT_REG, KT_H, 0, 0, kt_and_reg8, 1, 4 },
    { 0xA5, "AND", KT_REG, KT_L, 0, 0, kt_and_reg8, 1, 4 },
    { 0xA6, "AND", KT_REG_IND, KT_HL, 0, 0, kt_and_hl_ind, 1, 8 },
    { 0xA7, "AND", KT_REG, KT_A, 0, 0, kt_and_reg8, 1, 4 },
    { 0xA8, "XOR", KT_REG, KT_B, 0, 0, kt_xor_reg8, 1, 4 },
    { 0xA9, "XOR", KT_REG, KT_C, 0, 0, kt_xor_reg8, 1, 4 },
    { 0xAA, "XOR", KT_REG, KT_D, 0, 0, kt_xor_reg8, 1, 4 },
    { 0xAB, "XOR", KT_REG, KT_E, 0, 0, kt_xor_reg8, 1, 4 },
    { 0xAC, "XOR", KT_REG, KT_H, 0, 0, kt_xor_reg8, 1, 4 },
    { 0xAD, "XOR", KT_REG, KT_L, 0, 0, kt_xor_reg8, 1, 4 },
    { 0xAE, "XOR", KT_REG_IND, KT_HL, 0, 0, kt_xor_hl_ind, 1, 8 },
    { 0xAF, "XOR", KT_REG, KT_A, 0, 0, kt_xor_reg8, 1, 8 },
    { 0xB0, "OR", KT_REG, KT_B, 0, 0, kt_or_reg8, 1, 4 },
    { 0xB1, "OR", KT_REG, KT_C, 0, 0, kt_or_reg8, 1, 4 },
    { 0xB2, "OR", KT_REG, KT_D, 0, 0, kt_or_reg8, 1, 4 },
    { 0xB3, "OR", KT_REG, KT_E, 0, 0, kt_or_reg8, 1, 4 },
    { 0xB4, "OR", KT_REG, KT_H, 0, 0, kt_or_reg8, 1, 4 },
    { 0xB5, "OR", KT_REG, KT_L, 0, 0, kt_or_reg8, 1, 4 },
    { 0xB6, "OR", KT_REG_IND, KT_HL, 0, 0, kt_or_hl_ind, 1, 8 },
    { 0xB7, "OR", KT_REG, KT_A, 0, 0, kt_or_reg8, 1, 4 },
    { 0xB8, "CP", KT_REG, KT_B, 0, 0, kt_cp_reg8, 1, 4 },
    { 0xB9, "CP", KT_REG, KT_C, 0, 0, kt_cp_reg8, 1, 4 },
    { 0xBA, "CP", KT_REG, KT_D, 0, 0, kt_cp_reg8, 1, 4 },
    { 0xBB, "CP", KT_REG, KT_E, 0, 0, kt_cp_reg8, 1, 4 },
    { 0xBC, "CP", KT_REG, KT_H, 0, 0, kt_cp_reg8, 1, 4 },
    { 0xBD, "CP", KT_REG, KT_H, 0, 0, kt_cp_reg8, 1, 4 },
    { 0xBE, "CP", KT_REG_IND, KT_HL, 0, 0, kt_cp_hl_ind, 1, 8 },
    { 0xBF, "CP", KT_REG, KT_A, 0, 0, kt_cp_reg8, 1, 4 },
    { 0xC0, "RET NZ", 0, 0, 0, 0, kt_ret_nz, 1, 8 },
    { 0xC1, "POP", KT_REG, KT_BC, 0, 0, kt_pop_reg16, 1, 12 },
    { 0xC2, "JP NZ", KT_IMM16, 0, 0, 0, kt_jp_nz_imm16, 3, 12 },
    { 0xC3, "JP", KT_IMM16, 0, 0, 0, kt_jp_imm16, 3, 16 },
    { 0xC4, "CALL NZ", KT_IMM16, 0, 0, 0, kt_call_nz_imm16, 3, 12 },
    { 0xC5, "PUSH", KT_REG, KT_BC, 0, 0, kt_push_reg16, 1, 16 },
    { 0xC6, "ADD", KT_REG, KT_A, KT_IMM8, 0, kt_add_a_imm8, 2, 8 },
    { 0xC7, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xC8, "RET Z", 0, 0, 0, 0, kt_ret_z, 1, 8 },
    { 0xC9, "RET", 0, 0, 0, 0, kt_ret, 1, 16 },
    { 0xCA, "JP Z", KT_IMM16, 0, 0, 0, kt_jp_z_imm16, 3, 12 },
    { 0xCB, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xCC, "CALL Z", KT_IMM16, 0, 0, 0, kt_call_z_imm16, 3, 12 },
    { 0xCD, "CALL", KT_IMM16, 0, 0, 0, kt_call_imm16, 3, 24 },
    { 0xCE, "ADC", KT_REG, KT_A, KT_IMM8, 0, kt_adc_a_imm8, 2, 8 },
    { 0xCF, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xD0, "RET", 0, 0, 0, 0, kt_ret_nc, 1, 8 },
    { 0xD1, "POP", KT_REG, KT_DE, 0, 0, kt_pop_reg16, 1, 12 },
    { 0xD2, "JP NC", KT_IMM16, 0, 0, 0, kt_jp_nc_imm16, 3, 12 },
    { 0xD3, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xD4, "CALL NC", KT_IMM16, 0, 0, 0, kt_call_nc_imm16, 3, 12 },
    { 0xD5, "PUSH", KT_REG, KT_DE, 0, 0, kt_push_reg16, 1, 16 },
    { 0xD6, "SUB", KT_IMM8, 0, 0, 0, kt_sub_imm8, 2, 8 },
    { 0xD7, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xD8, "RET C", 0, 0, 0, 0, kt_ret_c, 1, 8 },
    { 0xD9, "RETI", 0, 0, 0, 0, kt_reti, 1, 16 },
    { 0xDA, "JP C", KT_IMM16, 0, 0, 0, kt_jp_c_imm16, 3, 12 },
    { 0xDB, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xDC, "CALL C", KT_IMM16, 0, 0, 0, kt_call_c_imm16, 1, 12 },
    { 0xDD, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xDE, "SBC", KT_REG, KT_A, KT_IMM8, 0, kt_sbc_a_imm8, 2, 8 },
    { 0xDF, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xE0, "LD", KT_HIMEM8_IND, 0, KT_REG, KT_A,
        kt_ld_himem8_ind_reg8, 2, 12 },
    { 0xE1, "POP", KT_REG, KT_HL, 0, 0, kt_pop_reg16, 1, 12 },
    { 0xE2, "LD", KT_REG_IND, KT_C, KT_REG, KT_A,
        kt_ld_reg8_ind_reg8, 1, 8 }, /* pastraiser is wrong, it's 1 byte */
    { 0xE3, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xE4, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xE5, "PUSH", KT_REG, KT_HL, 0, 0, kt_push_reg16, 1, 16 },
    { 0xE6, "AND", KT_IMM8, 0, 0, 0, kt_and_imm8, 2, 8 },
    { 0xE7, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xE8, "ADD", KT_REG, KT_SP, KT_SIMM8, 0, kt_add_sp_simm8, 2, 16 },
    { 0xE9, "JP", KT_REG_IND, KT_HL, 0, 0, kt_jp_hl_ind, 1, 4 },
    { 0xEA, "LD", KT_IMM16_IND, 0, KT_REG, KT_A,
        kt_ld_imm16_ind_reg8, 3, 16 },
    { 0xEB, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xEC, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xED, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xEE, "XOR", KT_IMM8, 0, 0, 0, kt_xor_imm8, 2, 8 },
    { 0xEF, "RST", KT_LOMEM, 0x28, 0, 0, kt_rst_lomem, 1, 16 },
    { 0xF0, "LD", KT_REG, KT_A, KT_HIMEM8_IND, 0,
        kt_ld_reg8_himem8_ind, 2, 12 },
    { 0xF1, "POP", KT_REG, KT_AF, 0, 0, kt_pop_af, 1, 12 },
    { 0xF2, "LD", KT_REG, KT_A, KT_REG_IND, KT_C,
        kt_ld_reg8_reg8_ind, 2, 8 },
    { 0xF3, "DI", 0, 0, 0, 0, kt_di, 1, 4 },
    { 0xF4, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xF5, "PUSH", KT_REG, KT_AF, 0, 0, kt_push_reg16, 1, 16 },
    { 0xF6, "OR", KT_IMM8, 0, 0, 0, kt_or_imm8, 2, 8 },
    { 0xF7, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xF8, "LD", KT_REG, KT_HL, KT_SPREL8, 0, kt_ld_hl_sprel8, 2, 12 },
    { 0xF9, "LD", KT_REG, KT_SP, KT_REG, KT_HL, kt_ld_sp_hl, 1, 8 },
    { 0xFA, "LD", KT_REG, KT_A, KT_IMM16_IND, 0,
        kt_ld_reg8_imm16_ind, 3, 16 },
    { 0xFB, "EI", 0, 0, 0, 0, kt_ei, 1, 4 },
    { 0xFC, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xFD, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 1, 0 },
    { 0xFE, "CP", KT_IMM8, 0, 0, 0, kt_cp_imm8, 2, 8 },
    { 0xFF, "RST", KT_LOMEM, 0x38, 0, 0, kt_rst_lomem, 1, 16 },

    /* ------------------------------------------------------------- */

    { 0x00, "RLC", KT_REG, KT_B, 0, 0, kt_rlc_reg8, 2, 8 },
    { 0x01, "RLC", KT_REG, KT_C, 0, 0, kt_rlc_reg8, 2, 8 },
    { 0x02, "RLC", KT_REG, KT_D, 0, 0, kt_rlc_reg8, 2, 8 },
    { 0x03, "RLC", KT_REG, KT_E, 0, 0, kt_rlc_reg8, 2, 8 },
    { 0x04, "RLC", KT_REG, KT_H, 0, 0, kt_rlc_reg8, 2, 8 },
    { 0x05, "RLC", KT_REG, KT_L, 0, 0, kt_rlc_reg8, 2, 8 },
    { 0x06, "RLC", KT_REG_IND, KT_HL, 0, 0, kt_rlc_hl_ind, 2, 16 },
    { 0x07, "RLC", KT_REG, KT_A, 0, 0, kt_rlc_reg8, 2, 8 },
    { 0x08, "RRC", KT_REG, KT_B, 0, 0, kt_rrc_reg8, 2, 8 },
    { 0x09, "RRC", KT_REG, KT_C, 0, 0, kt_rrc_reg8, 2, 8 },
    { 0x0A, "RRC", KT_REG, KT_D, 0, 0, kt_rrc_reg8, 2, 8 },
    { 0x0B, "RRC", KT_REG, KT_E, 0, 0, kt_rrc_reg8, 2, 8 },
    { 0x0C, "RRC", KT_REG, KT_H, 0, 0, kt_rrc_reg8, 2, 8 },
    { 0x0D, "RRC", KT_REG, KT_L, 0, 0, kt_rrc_reg8, 2, 8 },
    { 0x0E, "RRC", KT_REG_IND, KT_HL, 0, 0, kt_rrc_hl_ind, 2, 16 },
    { 0x0F, "RRC", KT_REG, KT_A, 0, 0, kt_rrc_reg8, 2, 8 },
    { 0x10, "RL", KT_REG, KT_B, 0, 0, kt_rl_reg8, 2, 8 },
    { 0x11, "RL", KT_REG, KT_C, 0, 0, kt_rl_reg8, 2, 8 },
    { 0x12, "RL", KT_REG, KT_D, 0, 0, kt_rl_reg8, 2, 8 },
    { 0x13, "RL", KT_REG, KT_E, 0, 0, kt_rl_reg8, 2, 8 },
    { 0x14, "RL", KT_REG, KT_H, 0, 0, kt_rl_reg8, 2, 8 },
    { 0x15, "RL", KT_REG, KT_L, 0, 0, kt_rl_reg8, 2, 8 },
    { 0x16, "RL", KT_REG_IND, KT_HL, 0, 0, kt_rl_hl_ind, 2, 16 },
    { 0x17, "RL", KT_REG, KT_A, 0, 0, kt_rl_reg8, 2, 8 },
    { 0x18, "RR", KT_REG, KT_B, 0, 0, kt_rr_reg8, 2, 8 },
    { 0x19, "RR", KT_REG, KT_C, 0, 0, kt_rr_reg8, 2, 8 },
    { 0x1A, "RR", KT_REG, KT_D, 0, 0, kt_rr_reg8, 2, 8 },
    { 0x1B, "RR", KT_REG, KT_E, 0, 0, kt_rr_reg8, 2, 8 },
    { 0x1C, "RR", KT_REG, KT_H, 0, 0, kt_rr_reg8, 2, 8 },
    { 0x1D, "RR", KT_REG, KT_L, 0, 0, kt_rr_reg8, 2, 8 },
    { 0x1E, "RR", KT_REG_IND, KT_HL, 0, 0, kt_rr_hl_ind, 2, 16 },
    { 0x1F, "RR", KT_REG, KT_A, 0, 0, kt_rr_reg8, 2, 8 },
    { 0x20, "SLA", KT_REG, KT_B, 0, 0, kt_sla_reg8, 2, 8 },
    { 0x21, "SLA", KT_REG, KT_C, 0, 0, kt_sla_reg8, 2, 8 },
    { 0x22, "SLA", KT_REG, KT_D, 0, 0, kt_sla_reg8, 2, 8 },
    { 0x23, "SLA", KT_REG, KT_E, 0, 0, kt_sla_reg8, 2, 8 },
    { 0x24, "SLA", KT_REG, KT_H, 0, 0, kt_sla_reg8, 2, 8 },
    { 0x25, "SLA", KT_REG, KT_L, 0, 0, kt_sla_reg8, 2, 8 },
    { 0x26, "SLA", KT_REG_IND, KT_HL, 0, 0, kt_sla_hl_ind, 2, 16 },
    { 0x27, "SLA", KT_REG, KT_A, 0, 0, kt_sla_reg8, 2, 8 },
    { 0x28, "SRA", KT_REG, KT_B, 0, 0, kt_sra_reg8, 2, 8 },
    { 0x29, "SRA", KT_REG, KT_C, 0, 0, kt_sra_reg8, 2, 8 },
    { 0x2A, "SRA", KT_REG, KT_D, 0, 0, kt_sra_reg8, 2, 8 },
    { 0x2B, "SRA", KT_REG, KT_E, 0, 0, kt_sra_reg8, 2, 8 },
    { 0x2C, "SRA", KT_REG, KT_H, 0, 0, kt_sra_reg8, 2, 8 },
    { 0x2D, "SRA", KT_REG, KT_L, 0, 0, kt_sra_reg8, 2, 8 },
    { 0x2E, "SRA", KT_REG_IND, KT_HL, 0, 0, kt_sra_hl_ind, 2, 16 },
    { 0x2F, "SRA", KT_REG, KT_A, 0, 0, kt_sra_reg8, 2, 8 },
    { 0x30, "SWAP", KT_REG, KT_B, 0, 0, kt_swap_reg8, 2, 8 },
    { 0x31, "SWAP", KT_REG, KT_C, 0, 0, kt_swap_reg8, 2, 8 },
    { 0x32, "SWAP", KT_REG, KT_D, 0, 0, kt_swap_reg8, 2, 8 },
    { 0x33, "SWAP", KT_REG, KT_E, 0, 0, kt_swap_reg8, 2, 8 },
    { 0x34, "SWAP", KT_REG, KT_H, 0, 0, kt_swap_reg8, 2, 8 },
    { 0x35, "SWAP", KT_REG, KT_L, 0, 0, kt_swap_reg8, 2, 8 },
    { 0x36, "LD", KT_REG_IND, KT_HL, KT_IMM8, 0,
        kt_ld_reg16_ind_imm8, 2, 12 },
    { 0x37, "SWAP", KT_REG, KT_A, 0, 0, kt_swap_reg8, 2, 8 },
    { 0x38, "SRL", KT_REG, KT_B, 0, 0, kt_srl_reg8, 2, 8 },
    { 0x39, "SRL", KT_REG, KT_C, 0, 0, kt_srl_reg8, 2, 8 },
    { 0x3A, "SRL", KT_REG, KT_D, 0, 0, kt_srl_reg8, 2, 8 },
    { 0x3B, "SRL", KT_REG, KT_E, 0, 0, kt_srl_reg8, 2, 8 },
    { 0x3C, "SRL", KT_REG, KT_H, 0, 0, kt_srl_reg8, 2, 8 },
    { 0x3D, "SRL", KT_REG, KT_L, 0, 0, kt_srl_reg8, 2, 8 },
    { 0x3E, "SRL", KT_REG_IND, KT_HL, 0, 0, kt_srl_hl_ind, 2, 16 },
    { 0x3F, "SRL", KT_REG, KT_A, 0, 0, kt_srl_reg8, 2, 8 },
    { 0x40, "BIT", KT_REG, KT_B, KT_BIT, 0, kt_bit_reg8, 2, 8 },
    { 0x41, "BIT", KT_REG, KT_C, KT_BIT, 0, kt_bit_reg8, 2, 8 },
    { 0x42, "BIT", KT_REG, KT_D, KT_BIT, 0, kt_bit_reg8, 2, 8 },
    { 0x43, "BIT", KT_REG, KT_E, KT_BIT, 0, kt_bit_reg8, 2, 8 },
    { 0x44, "BIT", KT_REG, KT_H, KT_BIT, 0, kt_bit_reg8, 2, 8 },
    { 0x45, "BIT", KT_REG, KT_L, KT_BIT, 0, kt_bit_reg8, 2, 8 },
    { 0x46, "BIT", KT_REG_IND, KT_HL, KT_BIT, 0, kt_bit_hl_ind, 2, 16 },
    { 0x47, "BIT", KT_REG, KT_A, KT_BIT, 0, kt_bit_reg8, 2, 8 },
    { 0x48, "BIT", KT_REG, KT_B, KT_BIT, 1, kt_bit_reg8, 2, 8 },
    { 0x49, "BIT", KT_REG, KT_C, KT_BIT, 1, kt_bit_reg8, 2, 8 },
    { 0x4A, "BIT", KT_REG, KT_D, KT_BIT, 1, kt_bit_reg8, 2, 8 },
    { 0x4B, "BIT", KT_REG, KT_E, KT_BIT, 1, kt_bit_reg8, 2, 8 },
    { 0x4C, "BIT", KT_REG, KT_H, KT_BIT, 1, kt_bit_reg8, 2, 8 },
    { 0x4D, "BIT", KT_REG, KT_L, KT_BIT, 1, kt_bit_reg8, 2, 8 },
    { 0x4E, "BIT", KT_REG_IND, KT_HL, KT_BIT, 1, kt_bit_hl_ind, 2, 16 },
    { 0x4F, "BIT", KT_REG, KT_A, KT_BIT, 1, kt_bit_reg8, 2, 8 },
    { 0x50, "BIT", KT_REG, KT_B, KT_BIT, 2, kt_bit_reg8, 2, 8 },
    { 0x51, "BIT", KT_REG, KT_C, KT_BIT, 2, kt_bit_reg8, 2, 8 },
    { 0x52, "BIT", KT_REG, KT_D, KT_BIT, 2, kt_bit_reg8, 2, 8 },
    { 0x53, "BIT", KT_REG, KT_E, KT_BIT, 2, kt_bit_reg8, 2, 8 },
    { 0x54, "BIT", KT_REG, KT_H, KT_BIT, 2, kt_bit_reg8, 2, 8 },
    { 0x55, "BIT", KT_REG, KT_L, KT_BIT, 2, kt_bit_reg8, 2, 8 },
    { 0x56, "BIT", KT_REG_IND, KT_HL, KT_BIT, 2, kt_bit_hl_ind, 2, 16 },
    { 0x57, "BIT", KT_REG, KT_A, KT_BIT, 2, kt_bit_reg8, 2, 8 },
    { 0x58, "BIT", KT_REG, KT_B, KT_BIT, 3, kt_bit_reg8, 2, 8 },
    { 0x59, "BIT", KT_REG, KT_C, KT_BIT, 3, kt_bit_reg8, 2, 8 },
    { 0x5A, "BIT", KT_REG, KT_D, KT_BIT, 3, kt_bit_reg8, 2, 8 },
    { 0x5B, "BIT", KT_REG, KT_E, KT_BIT, 3, kt_bit_reg8, 2, 8 },
    { 0x5C, "BIT", KT_REG, KT_H, KT_BIT, 3, kt_bit_reg8, 2, 8 },
    { 0x5D, "BIT", KT_REG, KT_L, KT_BIT, 3, kt_bit_reg8, 2, 8 },
    { 0x5E, "BIT", KT_REG_IND, KT_HL, KT_BIT, 3, kt_bit_hl_ind, 2, 16 },
    { 0x5F, "BIT", KT_REG, KT_A, KT_BIT, 3, kt_bit_reg8, 2, 8 },
    { 0x60, "BIT", KT_REG, KT_B, KT_BIT, 4, kt_bit_reg8, 2, 8 },
    { 0x61, "BIT", KT_REG, KT_C, KT_BIT, 4, kt_bit_reg8, 2, 8 },
    { 0x62, "BIT", KT_REG, KT_D, KT_BIT, 4, kt_bit_reg8, 2, 8 },
    { 0x63, "BIT", KT_REG, KT_E, KT_BIT, 4, kt_bit_reg8, 2, 8 },
    { 0x64, "BIT", KT_REG, KT_H, KT_BIT, 4, kt_bit_reg8, 2, 8 },
    { 0x65, "BIT", KT_REG, KT_L, KT_BIT, 4, kt_bit_reg8, 2, 8 },
    { 0x66, "BIT", KT_REG_IND, KT_HL, KT_BIT, 4, kt_bit_hl_ind, 2, 16 },
    { 0x67, "BIT", KT_REG, KT_A, KT_BIT, 4, kt_bit_reg8, 2, 8 },
    { 0x68, "BIT", KT_REG, KT_B, KT_BIT, 5, kt_bit_reg8, 2, 8 },
    { 0x69, "BIT", KT_REG, KT_C, KT_BIT, 5, kt_bit_reg8, 2, 8 },
    { 0x6A, "BIT", KT_REG, KT_D, KT_BIT, 5, kt_bit_reg8, 2, 8 },
    { 0x6B, "BIT", KT_REG, KT_E, KT_BIT, 5, kt_bit_reg8, 2, 8 },
    { 0x6C, "BIT", KT_REG, KT_H, KT_BIT, 5, kt_bit_reg8, 2, 8 },
    { 0x6D, "BIT", KT_REG, KT_L, KT_BIT, 5, kt_bit_reg8, 2, 8 },
    { 0x6E, "BIT", KT_REG_IND, KT_HL, KT_BIT, 5, kt_bit_hl_ind, 2, 16 },
    { 0x6F, "BIT", KT_REG, KT_A, KT_BIT, 5, kt_bit_reg8, 2, 8 },
    { 0x70, "BIT", KT_REG, KT_B, KT_BIT, 6, kt_bit_reg8, 2, 8 },
    { 0x71, "BIT", KT_REG, KT_C, KT_BIT, 6, kt_bit_reg8, 2, 8 },
    { 0x72, "BIT", KT_REG, KT_D, KT_BIT, 6, kt_bit_reg8, 2, 8 },
    { 0x73, "BIT", KT_REG, KT_E, KT_BIT, 6, kt_bit_reg8, 2, 8 },
    { 0x74, "BIT", KT_REG, KT_H, KT_BIT, 6, kt_bit_reg8, 2, 8 },
    { 0x75, "BIT", KT_REG, KT_L, KT_BIT, 6, kt_bit_reg8, 2, 8 },
    { 0x76, "BIT", KT_REG_IND, KT_HL, KT_BIT, 6, kt_bit_hl_ind, 2, 16 },
    { 0x77, "BIT", KT_REG, KT_A, KT_BIT, 6, kt_bit_reg8, 2, 8 },
    { 0x78, "BIT", KT_REG, KT_B, KT_BIT, 7, kt_bit_reg8, 2, 8 },
    { 0x79, "BIT", KT_REG, KT_C, KT_BIT, 7, kt_bit_reg8, 2, 8 },
    { 0x7A, "BIT", KT_REG, KT_D, KT_BIT, 7, kt_bit_reg8, 2, 8 },
    { 0x7B, "BIT", KT_REG, KT_E, KT_BIT, 7, kt_bit_reg8, 2, 8 },
    { 0x7C, "BIT", KT_REG, KT_H, KT_BIT, 7, kt_bit_reg8, 2, 8 },
    { 0x7D, "BIT", KT_REG, KT_L, KT_BIT, 7, kt_bit_reg8, 2, 8 },
    { 0x7E, "BIT", KT_REG_IND, KT_HL, KT_BIT, 7, kt_bit_hl_ind, 2, 16 },
    { 0x7F, "BIT", KT_REG, KT_A, KT_BIT, 7, kt_bit_reg8, 2, 8 },
    { 0x80, "RES", KT_REG, KT_B, KT_BIT, 0, kt_res_reg8, 2, 8 },
    { 0x81, "RES", KT_REG, KT_C, KT_BIT, 0, kt_res_reg8, 2, 8 },
    { 0x82, "RES", KT_REG, KT_D, KT_BIT, 0, kt_res_reg8, 2, 8 },
    { 0x83, "RES", KT_REG, KT_E, KT_BIT, 0, kt_res_reg8, 2, 8 },
    { 0x84, "RES", KT_REG, KT_H, KT_BIT, 0, kt_res_reg8, 2, 8 },
    { 0x85, "RES", KT_REG, KT_L, KT_BIT, 0, kt_res_reg8, 2, 8 },
    { 0x86, "RES", KT_REG_IND, KT_HL, KT_BIT, 0, kt_res_hl_ind, 2, 16 },
    { 0x87, "RES", KT_REG, KT_A, KT_BIT, 0, kt_res_reg8, 2, 8 },
    { 0x88, "RES", KT_REG, KT_B, KT_BIT, 1, kt_res_reg8, 2, 8 },
    { 0x89, "RES", KT_REG, KT_C, KT_BIT, 1, kt_res_reg8, 2, 8 },
    { 0x8A, "RES", KT_REG, KT_D, KT_BIT, 1, kt_res_reg8, 2, 8 },
    { 0x8B, "RES", KT_REG, KT_E, KT_BIT, 1, kt_res_reg8, 2, 8 },
    { 0x8C, "RES", KT_REG, KT_H, KT_BIT, 1, kt_res_reg8, 2, 8 },
    { 0x8D, "RES", KT_REG, KT_L, KT_BIT, 1, kt_res_reg8, 2, 8 },
    { 0x8E, "RES", KT_REG_IND, KT_HL, KT_BIT, 1, kt_res_hl_ind, 2, 16 },
    { 0x8F, "RES", KT_REG, KT_A, KT_BIT, 1, kt_res_reg8, 2, 8 },
    { 0x90, "RES", KT_REG, KT_B, KT_BIT, 2, kt_res_reg8, 2, 8 },
    { 0x91, "RES", KT_REG, KT_C, KT_BIT, 2, kt_res_reg8, 2, 8 },
    { 0x92, "RES", KT_REG, KT_D, KT_BIT, 2, kt_res_reg8, 2, 8 },
    { 0x93, "RES", KT_REG, KT_E, KT_BIT, 2, kt_res_reg8, 2, 8 },
    { 0x94, "RES", KT_REG, KT_H, KT_BIT, 2, kt_res_reg8, 2, 8 },
    { 0x95, "RES", KT_REG, KT_L, KT_BIT, 2, kt_res_reg8, 2, 8 },
    { 0x96, "RES", KT_REG_IND, KT_HL, KT_BIT, 2, kt_res_hl_ind, 2, 16 },
    { 0x97, "RES", KT_REG, KT_A, KT_BIT, 2, kt_res_reg8, 2, 8 },
    { 0x98, "RES", KT_REG, KT_B, KT_BIT, 3, kt_res_reg8, 2, 8 },
    { 0x99, "RES", KT_REG, KT_C, KT_BIT, 3, kt_res_reg8, 2, 8 },
    { 0x9A, "RES", KT_REG, KT_D, KT_BIT, 3, kt_res_reg8, 2, 8 },
    { 0x9B, "RES", KT_REG, KT_E, KT_BIT, 3, kt_res_reg8, 2, 8 },
    { 0x9C, "RES", KT_REG, KT_H, KT_BIT, 3, kt_res_reg8, 2, 8 },
    { 0x9D, "RES", KT_REG, KT_L, KT_BIT, 3, kt_res_reg8, 2, 8 },
    { 0x9E, "RES", KT_REG_IND, KT_HL, KT_BIT, 3, kt_res_hl_ind, 2, 16 },
    { 0x9F, "RES", KT_REG, KT_A, KT_BIT, 3, kt_res_reg8, 2, 8 },
    { 0xA0, "RES", KT_REG, KT_B, KT_BIT, 4, kt_res_reg8, 2, 8 },
    { 0xA1, "RES", KT_REG, KT_C, KT_BIT, 4, kt_res_reg8, 2, 8 },
    { 0xA2, "RES", KT_REG, KT_D, KT_BIT, 4, kt_res_reg8, 2, 8 },
    { 0xA3, "RES", KT_REG, KT_E, KT_BIT, 4, kt_res_reg8, 2, 8 },
    { 0xA4, "RES", KT_REG, KT_H, KT_BIT, 4, kt_res_reg8, 2, 8 },
    { 0xA5, "RES", KT_REG, KT_L, KT_BIT, 4, kt_res_reg8, 2, 8 },
    { 0xA6, "RES", KT_REG_IND, KT_HL, KT_BIT, 4, kt_res_hl_ind, 2, 16 },
    { 0xA7, "RES", KT_REG, KT_A, KT_BIT, 4, kt_res_reg8, 2, 8 },
    { 0xA8, "RES", KT_REG, KT_B, KT_BIT, 5, kt_res_reg8, 2, 8 },
    { 0xA9, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xAA, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xAB, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xAC, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xAD, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xAE, "RES", KT_REG_IND, KT_HL, KT_BIT, 5, kt_res_hl_ind, 2, 16 },
    { 0xAF, "RES", KT_REG, KT_A, KT_BIT, 5, kt_res_reg8, 2, 8 },
    { 0xB0, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xB1, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xB2, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xB3, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xB4, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xB5, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xB6, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xB7, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xB8, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xB9, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xBA, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xBB, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xBC, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xBD, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xBE, "RES", KT_REG_IND, KT_HL, KT_BIT, 7, kt_res_hl_ind, 2, 16 },
    { 0xBF, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC0, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC1, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC2, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC3, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC4, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC5, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC6, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC7, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC8, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xC9, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xCA, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xCB, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xCC, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xCD, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xCE, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xCF, "SET", KT_REG, KT_A, KT_BIT, 1, kt_set_reg8, 2, 8 },
    { 0xD0, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xD1, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xD2, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xD3, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xD4, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xD5, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xD6, "SET", KT_REG_IND, KT_HL, KT_BIT, 2, kt_set_hl_ind, 2, 16 },
    { 0xD7, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xD8, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xD9, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xDA, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xDB, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xDC, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xDD, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xDE, "SET", KT_REG_IND, KT_HL, KT_BIT, 3, kt_set_hl_ind, 2, 16 },
    { 0xDF, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE0, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE1, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE2, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE3, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE4, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE5, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE6, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE7, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE8, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xE9, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xEA, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xEB, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xEC, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xED, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xEE, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xEF, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xF0, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xF1, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xF2, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xF3, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xF4, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xF5, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xF6, "SET", KT_REG_IND, KT_HL, KT_BIT, 6, kt_set_hl_ind, 2, 16 },
    { 0xF7, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xF8, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xF9, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xFA, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xFB, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xFC, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xFD, "UNIMPLEMENTED", 0, 0, 0, 0, kt_unimplemented, 2, 0 },
    { 0xFE, "SET", KT_REG_IND, KT_HL, KT_BIT, 7, kt_set_hl_ind, 2, 16 },
    { 0xFF, "SET", KT_REG, KT_A, KT_BIT, 7, kt_set_reg8, 2, 8 },
};

/* --------------------------------------------------------------------- */

char* kt_reg_names[] = { "AF", "BC", "DE", "HL", "SP" };

int kt_print_reg(char* buf, int buf_len, int reg)
{
    char* name;

    name = kt_reg_names[reg >> 4];

    buf_len = SDL_max(0, buf_len);

    if (reg & KT_LO) {
        return sys_snprintf(buf, buf_len, "%c", name[1]);
    } else if (reg & KT_HI) {
        return sys_snprintf(buf, buf_len, "%c", name[0]);
    }

    return sys_snprintf(buf, buf_len, "%s", name);
}

int kt_print_operand(char* buf, int buf_len, kouta_t* kt, char* prefix,
    int addr, int operand, int instr_size)
{
    char* p;
    char* end;
    int opsize;
    int offset;

    if (kt_read(kt, kt->pc) == 0xCB) {
        opsize = 2;
    } else {
        opsize = 1;
    }

    p = buf;
    end = buf + buf_len;
    p += sys_snprintf(p, end - p, "%s", prefix);

    switch (addr)
    {
    case KT_REG:
        p += kt_print_reg(p, end - p, operand);
        break;
    case KT_REG_IND:
        p += sys_snprintf(p, end - p, "(");
        p += kt_print_reg(p, end - p, operand);
        p += sys_snprintf(p, end - p, ")");
        break;
    case KT_REG_IND_DEC:
        p += sys_snprintf(p, end - p, "(");
        p += kt_print_reg(p, end - p, operand);
        p += sys_snprintf(p, end - p, "-)");
        break;
    case KT_REG_IND_INC:
        p += sys_snprintf(p, end - p, "(");
        p += kt_print_reg(p, end - p, operand);
        p += sys_snprintf(p, end - p, "+)");
        break;
    case KT_IMM8:
        p += sys_snprintf(p, end - p, "%02X",
            kt_read(kt, kt->pc + opsize));
        break;
    case KT_SIMM8:
        offset = (Sint8)kt_read(kt, kt->pc + 1);
        p += sys_snprintf(p, end - p, "%s%02X",
            offset < 0 ? "-" : "", offset < 0 ? -offset : offset);
        break;
    case KT_REL8:
        p += sys_snprintf(p, end - p, "%04X",
            kt->pc + (Sint8)kt_read(kt, kt->pc + opsize) + instr_size);
        break;
    case KT_LOMEM:
         p += sys_snprintf(p, end - p, "%04X", operand);
         break;
    case KT_HIMEM8:
        p += sys_snprintf(p, end - p, "%04X",
            0xFF00 + kt_read(kt, kt->pc + opsize));
        break;
    case KT_HIMEM8_IND:
        p += sys_snprintf(p, end - p, "(%04X)",
            0xFF00 + kt_read(kt, kt->pc + opsize));
        break;
    case KT_SPREL8:
        offset = (Sint8)kt_read(kt, kt->pc + 1);
        p += sys_snprintf(p, end - p, "SP%c%02X",
            offset < 0 ? '-' : '+', offset < 0 ? -offset : offset);
        break;
    case KT_IMM16:
        p += sys_snprintf(p, end - p, "%04X",
            kt_read2(kt, kt->pc + opsize));
        break;
    case KT_IMM16_IND:
        p += sys_snprintf(p, end - p, "(%04X)",
            kt_read2(kt, kt->pc + opsize));
        break;
    case KT_BIT:
        p += sys_snprintf(p, end - p, "%d", operand);
        break;
    }

    return SDL_min(buf_len - 1, p - buf);
}

int kt_print_instruction(char* buf, int buf_len,
    kouta_t* kt, kt_op_t* instruction)
{
    char* p;
    char* end;
    int i;

    p = buf;
    end = buf + buf_len;

    p += sys_snprintf(p, end - p, "%04X: ", kt->pc);

    for (i = 0; i < instruction->size; ++i)
    {
        Uint8 b;

        b = kt_read(kt, kt->pc + i);
        p += sys_snprintf(p, end - p, "%02X ", b);
    }

    for (; i < 3; ++i) {
        p += sys_snprintf(p, end - p, "   ");
    }

    p += sys_snprintf(p, end - p, "    %s", instruction->name);

    if (instruction->addr_dst) {
        p += kt_print_operand(p, end - p, kt, " ", instruction->addr_dst,
            instruction->dst, instruction->size);
    }

    if (instruction->addr_src) {
        p += kt_print_operand(p, end - p, kt, ",", instruction->addr_src,
            instruction->src, instruction->size);
    }

    return SDL_min(buf_len - 1, p - buf);
}

/* --------------------------------------------------------------------- */

void kt_update_timer(kouta_t* kt, int delta_cycles)
{
    kt->div_cycles -= delta_cycles;

    if (kt->div_cycles <= 0) {
        kt->div_cycles += 0x100;
        ++kt->div;
    }

    if (kt->tac & KT_TAC_START)
    {
        kt->tima_cycles -= delta_cycles;

        if (kt->tima_cycles <= 0)
        {
            kt->tima_cycles += kt_tima_clocks[kt->tac & KT_TAC_CLOCK_BITS];

            if (kt->tima == 0xFF) {
                kt->tima = kt->tma;
                kt->if_ |= KT_INT_TIMER;
            } else {
                ++kt->tima;
            }
        }
    }
}

void kt_check_stat_int(kouta_t* kt, int interrupt)
{
    if (kt->stat & interrupt) {
        kt->if_ |= KT_INT_STAT;
    }
}

void kt_update_lcd(kouta_t* kt, int n_cycles)
{
    kt->entered_vblank = 0;
    kt->n_lcd_cycles += n_cycles;

    switch (kt->stat & KT_STAT_MODE_BITS)
    {
    case KT_STAT_HBLANK:
        if (kt->n_lcd_cycles >= 201)
        {
            kt->stat &= ~KT_STAT_MODE_BITS;
            kt->stat |= KT_STAT_READING_OAM;
            kt_check_stat_int(kt, KT_STAT_INT_OAM);
            kt->n_lcd_cycles -= 201;
        }
        break;

    case KT_STAT_VBLANK:
        kt->ly = SDL_min(153, KT_HEIGHT + kt->n_lcd_cycles / 506);

        if (kt->n_lcd_cycles >= 4560)
        {
            kt->stat &= ~KT_STAT_MODE_BITS;
            kt->stat |= KT_STAT_READING_OAM;
            kt_check_stat_int(kt, KT_STAT_INT_OAM);
            kt->n_lcd_cycles -= 4560;
            kt->ly = 0;
        }
        break;

    case KT_STAT_READING_OAM:
        if (kt->n_lcd_cycles >= 77)
        {
            kt->stat &= ~KT_STAT_MODE_BITS;
            kt->stat |= KT_STAT_READING_OAM_VRAM;
            kt->n_lcd_cycles -= 77;
        }
        break;

    case KT_STAT_READING_OAM_VRAM:
        if (kt->n_lcd_cycles >= 169)
        {
            ++kt->ly;
            kt->stat &= ~KT_STAT_MODE_BITS;
            kt->n_lcd_cycles -= 169;

            if (kt->ly == kt->lyc) {
                kt->stat |= KT_STAT_COINCIDENCE;
                kt_check_stat_int(kt, KT_STAT_INT_COINCIDENCE);
            } else {
                kt->stat &= ~KT_STAT_COINCIDENCE;
            }

            if (kt->ly >= KT_HEIGHT) {
                kt->stat |= KT_STAT_VBLANK;
                kt->if_ |= KT_INT_VBLANK;
                kt_check_stat_int(kt, KT_STAT_INT_VBLANK);
                kt->entered_vblank = 1;
            } else {
                kt->stat |= KT_STAT_HBLANK;
                kt_check_stat_int(kt, KT_STAT_INT_HBLANK);
            }
        }
        break;
    }
}

int kt_check_interrupt(kouta_t* kt, int interrupt, Uint16 vector)
{
    if (!(kt->ie & interrupt) || !(kt->if_ & interrupt)) {
        return 0;
    }

    kt->if_ &= ~interrupt;
    kt->ime = 0;
    kt_push2(kt, kt->pc);
    kt->pc = vector;
    kt->halt = 0;

#ifdef KT_DEBUG
    log_print(log_line, "*** INTERRUPT %02X ***", vector);
#endif

    return 1;
}

void kt_update_interrupts(kouta_t* kt)
{
    if (kt->ime_events & KT_IME_DI) {
        kt->ime = 0;
        kt->ime_events &= ~KT_IME_DI;
    }

    if (kt->ime_events & KT_IME_EI) {
        kt->ime = 1;
        kt->ime_events &= ~KT_IME_EI;
    }

    if (kt->ime)
    {
        if (kt_check_interrupt(kt, KT_INT_VBLANK, 0x40)) return;
        if (kt_check_interrupt(kt, KT_INT_STAT, 0x48)) return;
        if (kt_check_interrupt(kt, KT_INT_TIMER, 0x50)) return;
        if (kt_check_interrupt(kt, KT_INT_SERIAL, 0x58)) return;
        if (kt_check_interrupt(kt, KT_INT_JOYPAD, 0x60)) return;
    }
}

void kt_update_dma(kouta_t* kt, int delta_cycles)
{
    if (kt->dma_cycles <= 0) {
        return;
    }

    kt->dma_cycles -= delta_cycles;
}

int kt_tick(kouta_t* kt)
{
    int res;
    int op;
    kt_op_t* instruction;
    int start;
    char buf[1024];

    if (kt->error) {
        return 0;
    }

    kt_update_interrupts(kt);

    start = kt->n_cycles;

    if (kt->halt) {
        kt->n_cycles += 40;
        goto skip_instruction;
    }

    if (kt->stop)
    {
        if (kt->buttons) {
            kt->stop = 0;
        } else {
            kt->n_cycles += 40;
            goto skip_instruction;
        }
    }

    op = kt_read(kt, kt->pc);
    if (op == 0xCB) {
        op = kt_read(kt, kt->pc + 1) + 0x100;
    }

    instruction = &kt_op_table[op];
#ifdef KT_DEBUG
    SDL_Log("-----------------------------------------------------------");
    kt_print_regs(kt);
    kt_print_instruction(buf, sizeof(buf), kt, instruction);
    SDL_Log("%s", buf);
#endif

    instruction->execute(kt, instruction);

    if (!kt->error)
    {
        kt->pc += instruction->size;
        kt->n_cycles += instruction->n_cycles;

        if (instruction->n_cycles <= 0 || instruction->size <= 0) {
            log_puts("*** FIX YOUR CODE IDIOT ***");
            log_dump("d", instruction->n_cycles);
            log_dump("d", instruction->size);
            kt->error = 1;
        }
    }

    if (kt->error)
    {
        kt_print_regs(kt);
        kt_print_instruction(buf, sizeof(buf), kt, instruction);
        SDL_Log("%s", buf);
    }

skip_instruction:
    res = kt->n_cycles - start;

    kt_update_dma(kt, res);
    kt_update_timer(kt, res);
    kt_update_lcd(kt, res);

    return res;
}

/* --------------------------------------------------------------------- */

int r_display;
int r_window_mode;
int r_width;
int r_height;

SDL_Window* r_window;
SDL_Renderer* r_renderer;
SDL_Texture* r_texture;

void r_init()
{
    int flags;

    if (r_window) {
        SDL_SetWindowSize(r_window, r_width, r_height);
        return;
    }

    flags = 0;

    if (!r_window_mode)
    {
        SDL_DisplayMode mode;

        flags |= SDL_WINDOW_FULLSCREEN;

        if (!SDL_GetDesktopDisplayMode(r_display, &mode)) {
            r_width = mode.w;
            r_height = mode.h;
        }

        else {
            log_print(log_line, "SDL_GetDesktopDisplayMode failed: %s",
                SDL_GetError());
        }
    }

    r_window = SDL_CreateWindow("kouta",
        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
        r_width, r_height, flags);

    r_renderer = SDL_CreateRenderer(r_window, -1, SDL_RENDERER_SOFTWARE);
    r_texture = SDL_CreateTexture(r_renderer, SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_STREAMING, KT_WIDTH, KT_HEIGHT);
}

void r_begin(int** pix)
{
    int pitch;
    SDL_LockTexture(r_texture, 0, (void**)pix, &pitch);
}

void r_end()
{
    SDL_UnlockTexture(r_texture);
    SDL_RenderCopyEx(r_renderer, r_texture, 0, 0, 0, 0, SDL_FLIP_NONE);
    SDL_RenderPresent(r_renderer);
}

void r_clear(int* pix, int color)
{
    int i;

    for (i = 0; i < KT_WIDTH * KT_HEIGHT; ++i) {
        *pix++ = color;
    }
}

/* --------------------------------------------------------------------- */

kouta_t kt;
char* rom_file;

int running = 1;
int delta_cycles;
int frame_cycles;
int frames_per_second;
int cycles_per_second;
int skip_bootrom;
int fast_fwd;

int dmg_classic_palette[] = {
    0x9BBC0F,
    0x8BAC0F,
    0x306230,
    0x0F380F
};

int dmg_grayscale_palette[] = {
    0xFFFFFF,
    0xB6B6B6,
    0x676767,
    0x000000
};

int* dmg_palette = dmg_grayscale_palette;

/*
 * all the tile patterns, expanded to 1 byte per pixel
 *
 * laid out like:
 * tile0_row0 tile0_row1 tile0_row2 ... tile1_row0 tile1_row1 ...
 */
Uint8 tilemap[8 * 8 * 786];

int objs_by_row[(KT_HEIGHT + 16) * 40];
int n_objs_by_row[KT_HEIGHT + 16];

void print_usage(char* argv0)
{
    SDL_Log(
        "usage: %s [options] /path/to/rom.gb\n"
        "\n"
        "available options:\n"
        "    -window: window mode | default: off | example: -window\n"
        "    -d: main display index | default: 0 | example: -d 0\n"
        "    -w: window width | default: 160 | example: -w 800\n"
        "    -h: window height | default: 144 | example: -h 600\n"
        "    -classic: use classic color palette for dmg | -classic\n",
        argv0
    );
}

void parse_args(int argc, char* argv[])
{
    char* argv0;

    argv0 = argv[0];

    ++argv, --argc;

    for (; argc > 0; ++argv, --argc)
    {
        if (!strcmp(argv[0], "-window")) {
            r_window_mode = 1;
        }

        else if (!strcmp(argv[0], "-d") && argc >= 2) {
            r_display = SDL_atoi(argv[1]);
            ++argv, --argc;
        }

        else if (!strcmp(argv[0], "-w") && argc >= 2) {
            r_width = SDL_atoi(argv[1]);
            ++argv, --argc;
        }

        else if (!strcmp(argv[0], "-h") && argc >= 2) {
            r_height = SDL_atoi(argv[1]);
            ++argv, --argc;
        }

        else if (!strcmp(argv[0], "-classic")) {
            dmg_palette = dmg_classic_palette;
        }

        else if (!strcmp(argv[0], "-skip")) {
            skip_bootrom = 1;
        }

        else {
            break;
        }
    }

    if (argc >= 1) {
        rom_file = argv[0];
    } else {
        print_usage(argv[0]);
        exit(1);
    }

    if (r_width <= 0) {
        r_width = KT_WIDTH;
    }

    if (r_height <= 0) {
        r_height = KT_HEIGHT;
    }
}

void update_counters(float delta_time)
{
    static float one_second = 1;

    one_second -= delta_time;

    while (one_second <= 0)
    {
        log_dump("d", cycles_per_second);
        log_dump("d", frames_per_second);
        one_second = 1;
        cycles_per_second = 0;
        frames_per_second = 0;
    }
}

void init(int argc, char* argv[])
{
    parse_args(argc, argv);

    r_init();

    kt_reset(&kt);

    if (!kt_load_rom(&kt, rom_file)) {
        exit(1);
    }

    if (skip_bootrom) {
        kt_skip_bootrom(&kt);
    }
}

int bgp(int color)
{
    int mask;

    mask = 3 << (color * 2);
    return (kt.bgp & mask) >> (color * 2);
}

int obp(int color, int flags)
{
    Uint8 palette;
    int mask;

    palette = kt.obp[(flags & KT_OBJ_PALETTE >> 4) & 0x0F];
    mask = 3 << (color * 2);
    return (palette & mask) >> (color * 2);
}

void update_tilemap()
{
    Uint8* data;
    Uint8* tile;
    int i, x, y;

    if (kt.mode != KT_DMG_MODE) {
        log_puts("unimplemented rendering mode");
        exit(1);
    }

    for (i = 0; i < 384; ++i)
    {
        tile = &tilemap[i * 8 * 8];

        for (y = 0; y < 8; ++y)
        {
            data = &kt.vram[i * 16 + y * 2];

            for (x = 0; x < 8; ++x)
            {
                int msb, lsb;
                Uint8 color;

                lsb = data[0] & (1 << (7 - x));
                msb = data[1] & (1 << (7 - x));

                color = 0;
                if (lsb) color |= 1;
                if (msb) color |= 2;

                tile[y * 8 + x] = color;
            }
        }
    }
}

#define RENDER_OBJ 0x80000000

void render_tile(int* pix, Uint8* tiles, int n, int l, int t, int wrap,
    int flags)
{
    int x, y;

    if (l + 8 <= 0) {
        return;
    }

    if (l >= KT_WIDTH) {
        return;
    }

    if (t + 8 <= 0) {
        return;
    }

    if (t >= KT_HEIGHT) {
        return;
    }

    /*
     * NOTE: the bound checks aren't necessary for objs because the sorting
     * step already discards off-screen sprites, so performance could be
     * gained by getting rid of checks here
     */

    for (y = SDL_max(-t, 0) % 8; y < 8; ++y)
    {
        for (x = SDL_max(-l, 0) % 8; x < 8; ++x)
        {
            int* pixel;
            int wrapx, wrapy;
            int color;
            int src_x, src_y;

            wrapx = l + x;
            wrapy = t + y;

            if (wrap) {
                wrapx %= KT_WIDTH;
                wrapy %= KT_HEIGHT;
            }

            else if (wrapx >= KT_WIDTH || wrapy >= KT_HEIGHT) {
                continue;
            }

            pixel = &pix[wrapy * KT_WIDTH + wrapx];

            src_x = (flags & KT_OBJ_FLIP_X) ? 8 - x : x;
            src_y = (flags & KT_OBJ_FLIP_Y) ? 8 - y : y;
            color = tiles[n * 8 * 8 + src_y * 8 + src_x];

            if (color)
            {
                if (flags & RENDER_OBJ) {
                    *pixel = dmg_palette[obp(color, flags)];
                } else {
                    *pixel = dmg_palette[bgp(color)];
                }
            }
        }
    }
}

void render_map(int* pix, Uint8* map, Uint8 scx, Uint8 scy)
{
    int x, y;
    Uint8* tiles;
    int base_index;
    int starty, startx;
    int endy, endx;

    if (kt.lcdc & KT_LCDC_TILES) {
        /* 0-127 from 8000-87FF */
        /* 128-255 from 8800-8FFF */
        base_index = 0;
        tiles = tilemap;
    } else {
        /* 0-127 from 9000-97FF */
        /* 128-255 from 8800-8FFF */
        base_index = 128;
        tiles = &tilemap[128 * 8 * 8];
    }

    startx = SDL_max(0, scx / 8);
    starty = SDL_max(0, scy / 8);
    endx = SDL_max(0, (scx + KT_WIDTH) / 8 + 1);
    endy = SDL_max(0, (scy + KT_HEIGHT) / 8 + 1);

    for (y = starty; y < endy; ++y)
    {
        for (x = startx; x < endx; ++x)
        {
            int l, t;
            int tile_index;

            l = x * 8 - scx;
            t = y * 8 - scy;
            tile_index = (base_index + map[y * 32 + x]) % 0x100;
            render_tile(pix, tiles, tile_index, l, t, 1, 0);
        }
    }
}

void render_background(int* pix)
{
    Uint8* map;

    if (kt.lcdc & KT_LCDC_BG_MAP) {
        map = &kt.vram[0x1C00];
    } else {
        map = &kt.vram[0x1800];
    }

    render_map(pix, map, kt.scx, kt.scy);
}

void render_window(int* pix)
{
    Uint8* map;

    if (kt.lcdc & KT_LCDC_WINDOW_ENABLE)
    {
        if (kt.lcdc & KT_LCDC_WINDOW_MAP) {
            map = &kt.vram[0x1C00];
        } else {
            map = &kt.vram[0x1800];
        }

        render_map(pix, map, kt.wx, kt.wy);
    }
}

void render_obj(int* pix, int n, int flags_mask, int flags_match)
{
    Uint8* data;
    int x, y;
    int tile_index;
    int flags;

    data = &kt.oam[n * 4];
    y = data[0];
    x = data[1];
    tile_index = data[2];
    flags = data[3];

    if ((flags & flags_mask) != flags_match) {
        return;
    }

    flags |= RENDER_OBJ;

    if (kt.lcdc & KT_LCDC_OBJ_8x16)
    {
        int top, bottom;

        top = tile_index & (~1);
        bottom = tile_index | 1;

        if (flags & KT_OBJ_FLIP_Y)
        {
            int tmp;

            tmp = top;
            top = bottom;
            bottom = tmp;
        }

        render_tile(pix, tilemap, top, x - 8, y - 16, 0, flags);
        render_tile(pix, tilemap, bottom, x - 8, y - 8, 0, flags);

        return;
    }

    render_tile(pix, tilemap, tile_index, x - 8, y - 16, 0, flags);
}

/*
 * to emulate the gameboy's 10 object limit with z priority for the
 * leftmost objects we need to sort by x descending
 */
int compar_objs(void const* p1, void const* p2)
{
    int left, right;
    int lx, rx;

    left = *(int*)p1;
    right = *(int*)p2;

    lx = kt.oam[left * 4 + 1];
    rx = kt.oam[right * 4 + 1];

    return (int)rx - (int)lx;
}

void update_obj_list()
{
    int i;
    int row;

    memset(n_objs_by_row, 0, sizeof(n_objs_by_row));

    /* map objects by row */
    for (i = 0; i < 40; ++i)
    {
        Uint8* data;
        int y;
        int x;

        data = &kt.oam[i * 4];
        y = data[0];
        x = data[1];

        if (y <= 0 || y >= KT_HEIGHT + 16 || x <= 0 || x >= KT_WIDTH + 8) {
            continue;
        }

        objs_by_row[y * 40 + n_objs_by_row[y]++] = i;
    }

    /*
     * sort every row by x descending and take up to 10 objs
     * this is required to give priority to the leftmost objects
     * (objects with lower x appear above ones with higher x)
     */

    for (row = 0; row < KT_HEIGHT; ++row)
    {
        SDL_qsort(&objs_by_row[row * 40], n_objs_by_row[row],
            sizeof(int), compar_objs);
    }
}

void render_objs(int* pix, int flags_mask, int flags_match)
{
    int i;
    int row;

    for (row = 0; row < KT_HEIGHT; ++row)
    {
        for (i = 0; i < SDL_max(10, n_objs_by_row[row]); ++i)
        {
            render_obj(pix, objs_by_row[row * 40 + i], flags_mask,
                flags_match);
        }
    }
}

void render()
{
    int* pix;

    update_tilemap();
    update_obj_list();

    r_begin(&pix);
    r_clear(pix, dmg_palette[bgp(0)]);
    render_objs(pix, KT_OBJ_BEHIND, KT_OBJ_BEHIND);
    render_background(pix);
    render_window(pix);
    render_objs(pix, KT_OBJ_BEHIND, 0);
    r_end();

    ++frames_per_second;
}

void render_blank()
{
    int* pix;

    r_begin(&pix);
    r_clear(pix, dmg_palette[0]);
    r_end();
}

void tick()
{
    static int prev_ly = 0;

    delta_cycles = kt_tick(&kt);
    frame_cycles += delta_cycles;
    cycles_per_second += delta_cycles;

    if (kt.entered_vblank) {
        render();
    }

    else if (!(kt.lcdc & KT_LCDC_ENABLE) &&
        (kt.n_cycles % (kt.cycles_per_second / 60)) == 0)
    {
        render_blank();
    }

    prev_ly = kt.ly;

    if (kt.error) {
        running = 0;
    }
}

/* --------------------------------------------------------------------- */

void handle(SDL_Event* e)
{
    switch (e->type)
    {
    case SDL_QUIT:
        running = 0;
        break;

    case SDL_KEYDOWN:
        if (e->key.repeat) {
            break;
        }

        switch (e->key.keysym.sym)
        {
        case SDLK_ESCAPE:
            running = 0;
            break;
        case SDLK_z:
            kt.buttons |= KT_JOYP_A;
            break;
        case SDLK_x:
            kt.buttons |= KT_JOYP_B;
            break;
        case SDLK_RETURN:
            kt.buttons |= KT_JOYP_START;
            break;
        case SDLK_BACKSPACE:
            kt.buttons |= KT_JOYP_SELECT;
            break;
        case SDLK_LEFT:
            kt.dpad |= KT_JOYP_LEFT;
            break;
        case SDLK_RIGHT:
            kt.dpad |= KT_JOYP_RIGHT;
            break;
        case SDLK_UP:
            kt.dpad |= KT_JOYP_UP;
            break;
        case SDLK_DOWN:
            kt.dpad |= KT_JOYP_DOWN;
            break;
        case SDLK_SPACE:
            fast_fwd = 1;
            log_dump("d", fast_fwd);
            break;
        }
        break;

    case SDL_KEYUP:
        switch (e->key.keysym.sym)
        {
        case SDLK_ESCAPE:
            running = 0;
            break;
        case SDLK_z:
            kt.buttons &= ~KT_JOYP_A;
            break;
        case SDLK_x:
            kt.buttons &= ~KT_JOYP_B;
            break;
        case SDLK_RETURN:
            kt.buttons &= ~KT_JOYP_START;
            break;
        case SDLK_BACKSPACE:
            kt.buttons &= ~KT_JOYP_SELECT;
            break;
        case SDLK_LEFT:
            kt.dpad &= ~KT_JOYP_LEFT;
            break;
        case SDLK_RIGHT:
            kt.dpad &= ~KT_JOYP_RIGHT;
            break;
        case SDLK_UP:
            kt.dpad &= ~KT_JOYP_UP;
            break;
        case SDLK_DOWN:
            kt.dpad &= ~KT_JOYP_DOWN;
            break;
        case SDLK_SPACE:
            fast_fwd = 0;
            log_dump("d", fast_fwd);
            break;
        }
        break;
    }
}

int main(int argc, char* argv[])
{
    unsigned prev_ticks;

    SDL_Init(SDL_INIT_VIDEO);
    init(argc, argv);

    prev_ticks = SDL_GetTicks();

    while (running)
    {
        SDL_Event e;

        /* simulate instruction timings for every vblank */
        if (kt.entered_vblank)
        {
            unsigned ticks;
            float elapsed;
            float delta_time;

            /* cap tick rate to sdl's maximum timer resolution */
            for (; prev_ticks == SDL_GetTicks(); SDL_Delay(0));

            elapsed = (float)frame_cycles / kt.cycles_per_second;
            ticks = SDL_GetTicks();
            delta_time = (ticks - prev_ticks) * 0.001f;

            if (!fast_fwd)
            {
                while (1)
                {
                    ticks = SDL_GetTicks();
                    delta_time = (ticks - prev_ticks) * 0.001f;
                    if (delta_time >= elapsed) {
                        break;
                    }
                    SDL_Delay(0);
                }
            }

            update_counters(delta_time);
            prev_ticks = ticks;
            frame_cycles = 0;
        }

        for (; SDL_PollEvent(&e); handle(&e));

        tick();
    }

    return 0;
}
