#ifndef __DIGITAL_LOGGER_H__
#define __DIGITAL_LOGGER_H__

#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

typedef uint32_t LoggerColor;
enum
{
	LoggerColorBlack = 0,			///< Чёрный
	LoggerColorRed = 1,			///< Красный
	LoggerColorGreen = 2,			///< Зеленый
	LoggerColorYellow = 3,			///< Желтый
	LoggerColorBlue = 4,			///< Синий
	LoggerColorPurple = 5,			///< Фиолетовый
	LoggerColorTurquoise = 6,		///< Бирюзовый
	LoggerColorWhile = 7			///< Белый
	
};

/**
 * @brief Инициилизировать логгер
 */
int32_t InitLogger();

/**
 * @brief Деинициилизировать логгер
 */
void DeinitLogger();

/**
 * @brief Отправить сообщение
 * @param[in] id идентификатор сообщения (от 0 до 9)
 * @param[in] style Цвет сообщения (от 0 до 7). Если значение больше 7, то интерпретируется как RGB
 * @param[in] tag не используется
 */
void SendDigitLog(uint32_t id, LoggerColor style, uint32_t tag);

/**
 * @brief Сброс всех идентификаторов
 */
void ResetLogger();

#ifdef __cplusplus
}
#endif

#endif
